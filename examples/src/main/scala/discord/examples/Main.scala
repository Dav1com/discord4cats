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

package discord.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Env

/** Runs one of the examples: `ping` (default), `voice` or `voice-test`, chosen by the first argument or the
  * `EXAMPLE` environment variable (Scala.js `run` passes no arguments).
  */
object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    Env[IO].get("EXAMPLE").map(args.headOption.orElse(_).getOrElse("ping")).flatMap {
      case "ping"       => PingBot.run.as(ExitCode.Success)
      case "voice"      => VoiceBot.run.as(ExitCode.Success)
      case "voice-test" => VoiceTest.run.as(ExitCode.Success)
      case other        =>
        IO.println(s"Unknown example '$other'. Use 'ping', 'voice' or 'voice-test'.").as(ExitCode.Error)
    }
