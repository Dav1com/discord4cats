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

import cats.effect.{Fiber, IO, Ref}
import cats.effect.std.{Env, Mutex}
import cats.syntax.all.*
import discord.commands.{CommandContext, CommandRouter, Commands, Opt}
import discord.gateway.GatewayEvent
import discord.model.*
import discord.voice.{OggOpus, Voice, VoiceConnection}
import fs2.io.file.{Files, Path}

/** Plays `.opus` files in voice channels: `/join`, `/play file:<name>`, `/stop` and `/leave`.
  *
  * Files come from `AUDIO_DIR` (default `./audio`). Convert anything to Ogg Opus with
  * `ffmpeg -i song.mp3 -c:a libopus -b:a 96k -ar 48000 -ac 2 song.opus`.
  */
object VoiceBot:

  private final case class Session(
      connection: VoiceConnection[IO],
      leave: IO[Unit],
      playing: Option[Fiber[IO, Throwable, Unit]]
  )

  def run: IO[Unit] =
    Bot.resource(Intents.Guilds | Intents.GuildVoiceStates).use { bot =>
      for
        audioDir <- Env[IO].get("AUDIO_DIR").map(d => Path(d.getOrElse("audio")))
        // Where everyone is, from GUILD_CREATE and VOICE_STATE_UPDATE.
        voiceStates <- Ref.of[IO, Map[(Snowflake, Snowflake), Snowflake]](Map.empty)
        sessions <- Ref.of[IO, Map[Snowflake, Session]](Map.empty)
        lock <- Mutex[IO]

        trackVoiceStates = bot.gateway.events.evalMap {
          case GatewayEvent.GuildCreate(g) =>
            voiceStates.update(_ ++ g.voiceStates.flatMap(s => s.channelId.map(c => (g.id, s.userId) -> c)))
          case GatewayEvent.VoiceStateUpdate(s) =>
            s.guildId.traverse_(g =>
              voiceStates.update(m => s.channelId.fold(m - (g -> s.userId))(c => m + ((g -> s.userId) -> c)))
            )
          case _ => IO.unit
        }

        opusFiles = Files[IO]
          .list(audioDir)
          .filter(_.extName == ".opus")
          .map(_.fileName.toString)
          .compile
          .toList

        cmds = Commands[IO]

        stopPlaying = (guild: Snowflake) =>
          sessions
            .modify(m =>
              (m.updatedWith(guild)(_.map(_.copy(playing = None))), m.get(guild).flatMap(_.playing))
            )
            .flatMap(_.traverse_(_.cancel))

        leave = (guild: Snowflake) =>
          lock.lock.surround(
            stopPlaying(guild) >> sessions
              .modify(m => (m - guild, m.get(guild)))
              .flatMap(_.traverse_(_.leave))
          )

        join = (ctx: CommandContext[IO]) =>
          (ctx.guildId, ctx.interaction.invoker).tupled match
            case None                => IO.raiseError(new IllegalStateException("Use this in a server."))
            case Some((guild, user)) =>
              voiceStates.get.map(_.get(guild -> user.id)).flatMap {
                case None          => IO.raiseError(new IllegalStateException("Join a voice channel first."))
                case Some(channel) =>
                  lock.lock.surround(sessions.get.map(_.get(guild)).flatMap {
                    case Some(s) if s.connection.channelId == channel => IO.pure(s)
                    case existing                                     =>
                      existing.traverse_(_.leave) >>
                        Voice.join(bot.gateway, guild, channel, logger = Some(Bot.logger)).allocated.flatMap {
                          (conn, release) =>
                            val s = Session(conn, release, None)
                            sessions.update(_ + (guild -> s)).as(s)
                        }
                  })
              }

        /** Turns "Join a voice channel first." style failures into replies. */
        friendly = (ctx: CommandContext[IO], action: IO[Unit]) =>
          action.handleErrorWith {
            case e: IllegalStateException => ctx.reply(e.getMessage, ephemeral = true)
            case e                        => ctx.reply(s"Failed: ${e.getMessage}", ephemeral = true)
          }

        joinCmd = cmds.slash("join", "Join your voice channel") { ctx =>
          friendly(
            ctx,
            join(ctx).flatMap { s =>
              s.connection.daveProtocolVersion.flatMap { v =>
                val e2ee = if v > 0 then s" (end-to-end encrypted, DAVE v$v)" else ""
                ctx.reply(s"Joined <#${s.connection.channelId.asString}>$e2ee")
              }
            }
          )
        }

        playCmd = cmds
          .slash("play", "Play an .opus file", Opt.string("file", "File name")) { (ctx, file) =>
            friendly(
              ctx,
              opusFiles.flatMap { files =>
                if !files.contains(file) then ctx.reply(s"No such file: $file", ephemeral = true)
                else
                  for
                    s <- join(ctx)
                    guild = s.connection.guildId
                    _ <- stopPlaying(guild)
                    frames = Files[IO].readAll(audioDir / file).through(OggOpus.frames)
                    fiber <- (s.connection.play(frames) >> sessions
                      .update(_.updatedWith(guild)(_.map(_.copy(playing = None))))).start
                    _ <- sessions.update(_.updatedWith(guild)(_.map(_.copy(playing = Some(fiber)))))
                    _ <- ctx.reply(s"Playing **$file**")
                  yield ()
              }
            )
          }
          .autocomplete("file") { typed =>
            opusFiles.map(
              _.filter(_.toLowerCase.contains(typed.toLowerCase)).sorted
                .take(25)
                .map(f => ApplicationCommandOptionChoice.string(f, f))
            )
          }

        stopCmd = cmds.slash("stop", "Stop playing")(ctx =>
          ctx.guildId.traverse_(stopPlaying) >> ctx.reply("Stopped.")
        )

        leaveCmd = cmds.slash("leave", "Leave the voice channel")(ctx =>
          ctx.guildId.traverse_(leave) >> ctx.reply("Bye!")
        )

        router = CommandRouter(
          bot.rest,
          List(joinCmd, playCmd, stopCmd, leaveCmd).map(_.guildOnly),
          logger = Some(Bot.logger)
        )
        ready <- bot.gateway.ready
        _ <- Bot.logger.info(s"Logged in as ${ready.user.username}, playing files from $audioDir")
        _ <- router.register(ready.application.id, bot.devGuild)
        _ <- router
          .run(bot.gateway.events)
          .merge(trackVoiceStates.drain)
          .compile
          .drain
          .guarantee(sessions.get.flatMap(_.keys.toList.traverse_(leave)))
      yield ()
    }
