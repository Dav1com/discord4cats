/*
 * Copyright 2026 davicom
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package discord.rest

import cats.effect.{Ref, Resource, Temporal}
import cats.effect.std.Mutex
import cats.syntax.all.*
import io.circe.Json
import org.http4s.{Headers, Response, Status}
import org.http4s.circe.*
import org.typelevel.ci.*

import scala.concurrent.duration.*

/** Client-side implementation of Discord's rate limits.
  *
  *   - Requests to the same route and major parameter run one at a time.
  *   - Bucket state comes from the `X-RateLimit-*` headers. When a bucket is exhausted, the next request
  *     waits until it resets.
  *   - On a 429 the request is retried after `retry_after`. A global 429 pauses every route.
  */
final class RateLimiter[F[_]] private (
    routeBuckets: Ref[F, Map[String, String]],
    buckets: Ref[F, Map[String, RateLimiter.Bucket]],
    globalUntil: Ref[F, FiniteDuration],
    locks: Ref[F, Map[String, Mutex[F]]],
    maxRetries: Int
)(using F: Temporal[F]):
  import RateLimiter.*

  def run[A](route: Route)(call: Resource[F, Response[F]])(handle: Response[F] => F[A]): F[A] =
    lockFor(route).flatMap(_.lock.surround(attempt(route, call, handle, maxRetries)))

  private def attempt[A](
      route: Route,
      call: Resource[F, Response[F]],
      handle: Response[F] => F[A],
      retriesLeft: Int
  ): F[A] =
    waitForGlobal >> waitForBucket(route) >> call
      .use { resp =>
        update(route, resp.headers) >> {
          if resp.status == Status.TooManyRequests then limited(resp).map(Left(_))
          else handle(resp).map(Right(_))
        }
      }
      .flatMap {
        case Right(a) => F.pure(a)
        case Left(l)  =>
          val pause = l.retryAfter.max(0.seconds)
          val markGlobal = F.monotonic.flatMap(now => globalUntil.update(_.max(now + pause))).whenA(l.global)
          if retriesLeft > 0 then
            markGlobal >> F.sleep(pause) >> attempt(route, call, handle, retriesLeft - 1)
          else
            F.raiseError(DiscordError.RateLimited(route.toString, l.retryAfter.toMillis / 1000.0, l.global))
      }

  private def lockFor(route: Route): F[Mutex[F]] =
    locks.get.map(_.get(route.key)).flatMap {
      case Some(m) => F.pure(m)
      case None    =>
        Mutex[F].flatMap(m => locks.modify(ls => ls.get(route.key).fold((ls + (route.key -> m), m))(ls -> _)))
    }

  private def bucketKey(route: Route): F[String] =
    routeBuckets.get.map(_.get(templateKey(route)).fold(route.key)(hash => s"$hash ${route.major}"))

  private def waitForGlobal: F[Unit] =
    (globalUntil.get, F.monotonic).flatMapN((until, now) => F.sleep(until - now).whenA(until > now))

  private def waitForBucket(route: Route): F[Unit] =
    for
      key <- bucketKey(route)
      bucket <- buckets.get.map(_.get(key))
      now <- F.monotonic
      _ <- bucket match
        case Some(b) if b.remaining <= 0 && b.resetAt > now => F.sleep(b.resetAt - now)
        case _                                              => F.unit
    yield ()

  private def update(route: Route, headers: Headers): F[Unit] =
    val hash = headers.get(ci"X-RateLimit-Bucket").map(_.head.value)
    val remaining = headers.get(ci"X-RateLimit-Remaining").flatMap(_.head.value.toIntOption)
    val resetAfter = headers.get(ci"X-RateLimit-Reset-After").flatMap(_.head.value.toDoubleOption)
    val saveHash = hash.traverse_(h => routeBuckets.update(_ + (templateKey(route) -> h)))
    val saveBucket = (remaining, resetAfter).tupled.traverse_ { (rem, after) =>
      for
        now <- F.monotonic
        key <- bucketKey(route)
        _ <- buckets.update(_ + (key -> Bucket(rem, now + (after * 1000).toLong.millis)))
      yield ()
    }
    saveHash >> saveBucket

  private def limited(resp: Response[F]): F[Limited] =
    resp.attemptAs[Json].value.map { body =>
      val json = body.toOption
      val retryAfter = json
        .flatMap(_.hcursor.get[Double]("retry_after").toOption)
        .orElse(resp.headers.get(ci"Retry-After").flatMap(_.head.value.toDoubleOption))
        .getOrElse(1.0)
      val global = json.flatMap(_.hcursor.get[Boolean]("global").toOption).getOrElse(false) ||
        resp.headers.get(ci"X-RateLimit-Global").isDefined
      Limited((retryAfter * 1000).toLong.millis, global)
    }

object RateLimiter:
  private[rest] final case class Bucket(remaining: Int, resetAt: FiniteDuration)
  private final case class Limited(retryAfter: FiniteDuration, global: Boolean)

  private def templateKey(route: Route): String = s"${route.method.name} ${route.template}"

  def apply[F[_]: Temporal](maxRetries: Int = 3): F[RateLimiter[F]] =
    (
      Ref.of[F, Map[String, String]](Map.empty),
      Ref.of[F, Map[String, Bucket]](Map.empty),
      Ref.of[F, FiniteDuration](Duration.Zero),
      Ref.of[F, Map[String, Mutex[F]]](Map.empty)
    ).mapN(new RateLimiter(_, _, _, _, maxRetries))
