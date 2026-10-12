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

package discord.commands

import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import discord.gateway.GatewayEvent
import discord.model.*
import discord.rest.DiscordRest
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

/** Registers commands with Discord and dispatches interactions to them.
  *
  * {{{
  * val router = CommandRouter(rest, List(ping, play))
  * router.register(appId) >> router.run(gateway.events).compile.drain
  * }}}
  *
  * @param autoDeferAfter
  *   if a handler hasn't answered after this long, the router defers the response for it. Discord fails the
  *   interaction if there's no response within 3 seconds.
  * @param onComponent
  *   handles button/select menu interactions
  * @param onModalSubmit
  *   handles modal submissions
  */
final class CommandRouter[F[_]](
    rest: DiscordRest[F],
    commands: List[Command[F]],
    autoDeferAfter: FiniteDuration = 2500.millis,
    onComponent: CommandContext[F] => F[Unit],
    onModalSubmit: CommandContext[F] => F[Unit],
    logger: Logger[F]
)(using F: Temporal[F]):

  private val byKey: Map[(ApplicationCommandType, String), Command[F]] =
    commands.map(c => (c.commandType, c.name) -> c).toMap

  /** Overwrites the registered commands: globally, or in one guild (guild commands update instantly, which is
    * handy during development).
    */
  def register(applicationId: Snowflake, guildId: Option[Snowflake] = None): F[List[ApplicationCommand]] =
    val specs = commands.map(_.spec)
    guildId.fold(rest.bulkOverwriteGlobalCommands(applicationId, specs))(
      rest.bulkOverwriteGuildCommands(applicationId, _, specs)
    )

  /** Handles every `INTERACTION_CREATE` in `events`, concurrently. */
  def run(events: Stream[F, GatewayEvent]): Stream[F, Nothing] =
    events
      .collect { case GatewayEvent.InteractionCreate(i) => i }
      .parEvalMapUnorderedUnbounded(i =>
        handle(i).handleErrorWith(e => logger.error(e)("Interaction failed"))
      )
      .drain

  def handle(interaction: Interaction): F[Unit] =
    CommandContext(interaction, rest).flatMap { ctx =>
      interaction.`type` match
        case InteractionType.ApplicationCommand             => runCommand(ctx)
        case InteractionType.ApplicationCommandAutocomplete => autocomplete(ctx)
        case InteractionType.MessageComponent               => withAutoDefer(ctx, onComponent(ctx))
        case InteractionType.ModalSubmit                    => withAutoDefer(ctx, onModalSubmit(ctx))
        case InteractionType.Ping                           => ctx.respond(InteractionResponse.Pong)
        case other => logger.debug(s"Ignoring interaction type ${other.value}")
    }

  private def lookup(ctx: CommandContext[F]): Option[Command[F]] =
    for
      data <- ctx.interaction.data
      name <- data.name
      c <- byKey.get((data.`type`.getOrElse(ApplicationCommandType.ChatInput), name))
    yield c

  private def runCommand(ctx: CommandContext[F]): F[Unit] =
    lookup(ctx) match
      case None      => ctx.reply("Unknown command.", ephemeral = true)
      case Some(cmd) =>
        cmd.handle(ctx) match
          case Left(problem) => ctx.reply(problem, ephemeral = true)
          case Right(action) => withAutoDefer(ctx, action)

  private def autocomplete(ctx: CommandContext[F]): F[Unit] =
    lookup(ctx).flatMap(_.complete(ctx)).getOrElse(F.pure(Nil)).flatMap { choices =>
      ctx.respond(InteractionResponse.autocomplete(choices.take(25)))
    }

  private def withAutoDefer(ctx: CommandContext[F], action: F[Unit]): F[Unit] =
    val autoDefer =
      F.sleep(autoDeferAfter) >> ctx.defer().handleErrorWith(e => logger.warn(e)("Auto-defer failed"))
    autoDefer.background.surround(action).handleErrorWith { e =>
      logger.error(e)(s"Handler for interaction ${ctx.interaction.id.asString} failed") >>
        ctx.reply("Something went wrong while running this command.", ephemeral = true).attempt.void
    }

object CommandRouter:
  def apply[F[_]: Temporal](
      rest: DiscordRest[F],
      commands: List[Command[F]],
      autoDeferAfter: FiniteDuration = 2500.millis,
      onComponent: Option[CommandContext[F] => F[Unit]] = None,
      onModalSubmit: Option[CommandContext[F] => F[Unit]] = None,
      logger: Option[Logger[F]] = None
  ): CommandRouter[F] =
    new CommandRouter(
      rest,
      commands,
      autoDeferAfter,
      onComponent.getOrElse(_.defer()),
      onModalSubmit.getOrElse(_.defer()),
      logger.getOrElse(NoOpLogger[F])
    )
