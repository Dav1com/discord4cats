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

import cats.effect.{IO, Ref}
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import discord.model.*
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.{Message as _, *}
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.typelevel.ci.*

import scala.concurrent.duration.*

class RestSuite extends CatsEffectSuite:

  private val messageJson = Json.obj(
    "id" := "10",
    "channel_id" := "20",
    "author" := Json.obj("id" := "30", "username" := "bot"),
    "content" := "hello",
    "timestamp" := "2026-01-01T00:00:00Z"
  )

  private final case class Seen(method: Method, path: String, headers: Headers, body: String)

  /** A fake Discord: records requests and answers with `respond`. */
  private def fake(respond: Request[IO] => IO[Response[IO]]): IO[(DiscordRest[IO], Ref[IO, List[Seen]])] =
    for
      seen <- Ref.of[IO, List[Seen]](Nil)
      app = HttpApp[IO] { req =>
        req.bodyText.compile.string.flatMap { body =>
          seen.update(_ :+ Seen(req.method, req.uri.path.renderString, req.headers, body)) >> respond(req)
        }
      }
      rest <- DiscordRest[IO](Client.fromHttpApp(app), "secret-token")
    yield (rest, seen)

  test("createMessage posts JSON to the channel with the bot token") {
    fake(_ => Ok(messageJson)).flatMap { (rest, seen) =>
      rest.createMessage(Snowflake(20), CreateMessage.text("hello")).flatMap { msg =>
        seen.get.map { reqs =>
          assertEquals(msg.content, "hello")
          val r = reqs.head
          assertEquals(r.method, Method.POST)
          assertEquals(r.path, "/api/v10/channels/20/messages")
          assertEquals(r.headers.get(ci"Authorization").map(_.head.value), Some("Bot secret-token"))
          assertEquals(io.circe.parser.parse(r.body), Right(Json.obj("content" := "hello")))
        }
      }
    }
  }

  test("path segments are percent-encoded (emoji reactions)") {
    fake(_ => NoContent()).flatMap { (rest, seen) =>
      rest.createReaction(Snowflake(1), Snowflake(2), "👍") >> seen.get.map { reqs =>
        assertEquals(reqs.head.path, "/api/v10/channels/1/messages/2/reactions/%F0%9F%91%8D/@me")
      }
    }
  }

  test("edits only send the fields that change, and null clears") {
    fake(_ => Ok(messageJson)).flatMap { (rest, seen) =>
      rest.editMessage(
        Snowflake(1),
        Snowflake(2),
        EditMessage(content = Optional("x"), embeds = Optional.Null)
      ) >>
        seen.get.map(reqs =>
          assertEquals(
            io.circe.parser.parse(reqs.head.body),
            Right(Json.obj("content" := "x", "embeds" := Json.Null))
          )
        )
    }
  }

  test("file uploads are sent as multipart with payload_json") {
    fake(_ => Ok(messageJson)).flatMap { (rest, seen) =>
      val upload = FileUpload("a.txt", scodec.bits.ByteVector("hi".getBytes), "text/plain")
      rest.createMessage(Snowflake(1), CreateMessage.text("file").copy(files = List(upload))) >>
        seen.get.map { reqs =>
          val r = reqs.head
          assert(r.headers.get(ci"Content-Type").exists(_.head.value.startsWith("multipart/form-data")))
          assert(r.body.contains("name=\"payload_json\""), r.body)
          assert(r.body.contains("name=\"files[0]\"; filename=\"a.txt\""), r.body)
          assert(r.body.contains("\"attachments\":[{\"id\":0,\"filename\":\"a.txt\"}]"), r.body)
        }
    }
  }

  test("errors carry Discord's JSON error code") {
    fake(_ => NotFound(Json.obj("code" := 10008, "message" := "Unknown Message"))).flatMap { (rest, _) =>
      rest.getMessage(Snowflake(1), Snowflake(2)).attempt.map {
        case Left(e: DiscordError.Api) =>
          assertEquals(e.status, 404)
          assertEquals(e.code, Some(10008))
          assertEquals(e.discordMessage, "Unknown Message")
        case other => fail(s"Unexpected: $other")
      }
    }
  }

  test("an exhausted bucket makes the next request wait for the reset") {
    val program = for
      times <- Ref.of[IO, List[FiniteDuration]](Nil)
      rest <- fake { _ =>
        IO.monotonic.flatMap(now => times.update(_ :+ now)) >>
          Ok(messageJson).map(
            _.putHeaders(
              "X-RateLimit-Bucket" -> "abc",
              "X-RateLimit-Remaining" -> "0",
              "X-RateLimit-Reset-After" -> "2.5"
            )
          )
      }.map(_._1)
      _ <- rest.getMessage(Snowflake(1), Snowflake(2))
      _ <- rest.getMessage(Snowflake(1), Snowflake(3))
      ts <- times.get
    yield ts
    TestControl.executeEmbed(program).map { ts =>
      assertEquals(ts.size, 2)
      assert(ts(1) - ts(0) >= 2500.millis, ts)
    }
  }

  test("a 429 is retried after retry_after") {
    val program = for
      calls <- Ref.of[IO, Int](0)
      rest <- fake { _ =>
        calls.getAndUpdate(_ + 1).flatMap { n =>
          if n == 0 then
            TooManyRequests(Json.obj("message" := "slow down", "retry_after" := 1.5, "global" := false))
          else Ok(messageJson)
        }
      }.map(_._1)
      start <- IO.monotonic
      msg <- rest.getMessage(Snowflake(1), Snowflake(2))
      end <- IO.monotonic
      n <- calls.get
    yield (msg, end - start, n)
    TestControl.executeEmbed(program).map { (msg, elapsed, n) =>
      assertEquals(msg.content, "hello")
      assertEquals(n, 2)
      assert(elapsed >= 1500.millis, elapsed)
    }
  }

  test("a global 429 pauses other routes too") {
    val program = for
      calls <- Ref.of[IO, List[(String, FiniteDuration)]](Nil)
      first <- cats.effect.Deferred[IO, Unit]
      rest <- fake { req =>
        IO.monotonic
          .flatMap { now =>
            calls.modify(cs => (cs :+ (req.uri.path.renderString -> now), cs.isEmpty))
          }
          .flatMap { isFirst =>
            if isFirst then
              first.complete(()) >> TooManyRequests(
                Json.obj("message" := "global", "retry_after" := 3.0, "global" := true)
              )
            else Ok(messageJson)
          }
      }.map(_._1)
      a <- rest.getMessage(Snowflake(1), Snowflake(2)).start
      _ <- first.get
      _ <- IO.sleep(100.millis)
      _ <- rest.getMessage(Snowflake(9), Snowflake(9))
      _ <- a.joinWithNever
      cs <- calls.get
    yield cs
    TestControl.executeEmbed(program).map { cs =>
      val other = cs.find(_._1.contains("/channels/9/")).get
      assert(other._2 >= 3.seconds, cs)
    }
  }
