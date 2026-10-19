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

import cats.effect.{IO, Resource}
import cats.effect.std.Env
import discord.gateway.{Gateway, GatewayConfig, GatewayEvent}
import discord.model.*
import discord.rest.DiscordRest
import discord.voice.{OggOpus, Voice}
import fs2.io.file.{Files, Path}
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.*

/** Joins a voice channel, plays one file and leaves. No slash commands needed, so it's handy for testing
  * voice.
  *
  * Env: `DISCORD_TOKEN`, `DISCORD_GUILD_ID`, optionally `VOICE_CHANNEL_ID` (default: the voice channel with
  * the most people) and `VOICE_FILE` (default: the first `.opus` file in `AUDIO_DIR`, itself defaulting to
  * `./audio`).
  */
object VoiceTest:
  private val log = ConsoleLogger[IO](debugEnabled = true)

  private def env(name: String): IO[String] =
    Env[IO].get(name).flatMap(IO.fromOption(_)(new IllegalArgumentException(s"Set $name")))

  def run: IO[Unit] =
    val setup = for
      token <- Resource.eval(env("DISCORD_TOKEN"))
      guildId <- Resource.eval(env("DISCORD_GUILD_ID").map(Snowflake.parse(_).get))
      http <- EmberClientBuilder.default[IO].build
      rest <- Resource.eval(DiscordRest[IO](http, token))
      gateway <- Gateway[IO](rest, GatewayConfig(token, Intents.Guilds | Intents.GuildVoiceStates), Some(log))
      events <- gateway.subscribe
    yield (guildId, gateway, events)

    setup.use { (guildId, gateway, events) =>
      for
        guild <- events
          .collectFirst { case GatewayEvent.GuildCreate(g) if g.id == guildId => g }
          .compile
          .lastOrError
          .timeout(30.seconds)
        configured <- Env[IO].get("VOICE_CHANNEL_ID").map(_.flatMap(Snowflake.parse))
        voiceChannels = guild.channels.filter(_.`type` == ChannelType.GuildVoice)
        occupancy = guild.voiceStates.groupBy(_.channelId).view.mapValues(_.size).toMap
        channel <- IO.fromOption(
          configured
            .flatMap(id => guild.channels.find(_.id == id))
            .orElse(voiceChannels.maxByOption(c => occupancy.getOrElse(Some(c.id), 0)))
        )(new IllegalStateException(s"No voice channel in ${guild.name}"))
        dir <- Env[IO].get("AUDIO_DIR").map(d => Path(d.getOrElse("audio")))
        file <- Env[IO].get("VOICE_FILE").flatMap {
          case Some(f) => IO.pure(Path(f))
          case None    =>
            Files[IO]
              .list(dir)
              .filter(_.extName == ".opus")
              .compile
              .toList
              .map(_.sortBy(_.toString).headOption)
              .flatMap(IO.fromOption(_)(new IllegalStateException(s"No .opus file in $dir")))
        }
        _ <- log.info(
          s"Joining #${channel.name.getOrElse("?")} in ${guild.name} " +
            s"(${occupancy.getOrElse(Some(channel.id), 0)} people there) to play $file"
        )
        _ <- Voice.join(gateway, guildId, channel.id, logger = Some(log)).use { vc =>
          for
            v <- vc.daveProtocolVersion
            _ <- log.info(if v > 0 then s"DAVE v$v active"
            else "DAVE not active yet (waiting for the MLS group, or a stage channel)")
            // Give the MLS group a moment to form before audio starts.
            _ <- IO.sleep(2.seconds)
            v2 <- vc.daveProtocolVersion
            _ <- log.info(s"DAVE version before playing: $v2")
            start <- IO.monotonic
            _ <- vc.play(Files[IO].readAll(file).through(OggOpus.frames))
            end <- IO.monotonic
            _ <- log.info(s"Playback took ${(end - start).toMillis}ms")
          yield ()
        }
        _ <- log.info("Left the channel")
      yield ()
    }
