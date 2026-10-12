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

package discord.gateway.ws

import cats.effect.IO
import cats.effect.std.Env
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.Uri

import scala.concurrent.duration.*

/** Opt-in check against the real Discord gateway (no token needed): TLS, SNI and the WebSocket handshake. Run
  * with `DISCORD_LIVE_TESTS=1`.
  */
class LiveGatewaySuite extends CatsEffectSuite:
  override def munitIOTimeout = 30.seconds

  test("receives Hello from wss://gateway.discord.gg") {
    Env[IO].get("DISCORD_LIVE_TESTS").flatMap {
      case None    => IO(assume(false, "set DISCORD_LIVE_TESTS=1 to run"))
      case Some(_) =>
        WsClient[IO].connect(Uri.unsafeFromString("wss://gateway.discord.gg/?v=10&encoding=json")).use {
          conn =>
            conn.receive
              .collectFirst { case WsMessage.Text(t) => t }
              .compile
              .lastOrError
              .map { text =>
                val c = parse(text).toOption.get.hcursor
                assertEquals(c.get[Int]("op"), Right(10))
                assert(c.downField("d").get[Long]("heartbeat_interval").isRight, text)
              }
        }
    }
  }
