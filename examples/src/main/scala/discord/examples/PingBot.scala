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

import cats.effect.IO
import discord.commands.{CommandRouter, Commands}
import discord.gateway.GatewayEvent
import discord.model.Intents
import discord.rest.CreateMessage

/** Answers `!ping` messages and the `/ping` slash command.
  *
  * Reading message content needs the privileged Message Content intent, enabled in the developer portal.
  */
object PingBot:

  def run: IO[Unit] =
    Bot.resource(Intents.NonPrivileged | Intents.MessageContent).use { bot =>
      val cmds = Commands[IO]
      val ping = cmds.slash("ping", "Replies with pong")(ctx => ctx.reply(s"Pong, ${ctx.user.displayName}!"))
      val router = CommandRouter(bot.rest, List(ping), logger = Some(Bot.logger))

      val messages = bot.gateway.events
        .collect { case GatewayEvent.MessageCreate(m) if m.content == "!ping" && !m.author.bot => m }
        .evalMap(m => bot.rest.createMessage(m.channelId, CreateMessage.text("Pong!").replyTo(m)))
        .drain

      for
        ready <- bot.gateway.ready
        _ <- Bot.logger.info(s"Logged in as ${ready.user.username}")
        _ <- router.register(ready.application.id, bot.devGuild)
        _ <- router.run(bot.gateway.events).merge(messages).compile.drain
      yield ()
    }
