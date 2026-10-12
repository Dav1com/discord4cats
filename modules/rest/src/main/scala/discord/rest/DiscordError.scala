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

import io.circe.{Decoder, Json}

/** Errors raised by the REST client. */
sealed abstract class DiscordError(message: String) extends RuntimeException(message)

object DiscordError:

  /** Discord answered with a non-2xx status.
    *
    * @param code
    *   Discord's JSON error code (e.g. 10008 "Unknown Message"), when the body had one.
    * @param errors
    *   Field-level validation errors, for 400 responses.
    */
  final case class Api(
      route: String,
      status: Int,
      code: Option[Int],
      discordMessage: String,
      errors: Option[Json]
  ) extends DiscordError(
        s"$route failed with HTTP $status${code.fold("")(c => s" (code $c)")}: $discordMessage"
      )

  /** Still rate limited after exhausting retries. */
  final case class RateLimited(route: String, retryAfterSeconds: Double, global: Boolean)
      extends DiscordError(s"$route still rate limited (retry after ${retryAfterSeconds}s, global=$global)")

  /** The response body didn't match the expected shape. */
  final case class Decoding(route: String, cause: Throwable)
      extends DiscordError(s"Could not decode response of $route: ${cause.getMessage}")

  private[rest] final case class Body(code: Option[Int], message: Option[String], errors: Option[Json])
  private[rest] object Body:
    given Decoder[Body] = Decoder.forProduct3("code", "message", "errors")(Body.apply)
