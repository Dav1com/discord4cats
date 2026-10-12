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

import cats.syntax.all.*
import discord.model.*

/** A command that can be registered with Discord and dispatched by [[CommandRouter]]. Build them with
  * [[Commands]].
  */
sealed trait Command[F[_]]:
  def name: String
  def commandType: ApplicationCommandType
  def spec: ApplicationCommand
  private[commands] def handle(ctx: CommandContext[F]): Either[String, F[Unit]]
  private[commands] def complete(ctx: CommandContext[F]): Option[F[List[ApplicationCommandOptionChoice]]]

/** Settings shared by every top-level command. */
sealed trait CommandSettings[F[_], Self]:
  protected def settings: ApplicationCommand => ApplicationCommand
  protected def withSettings(f: ApplicationCommand => ApplicationCommand): Self

  /** Only members with these permissions see the command (admins can override this per guild). */
  def requirePermissions(p: Permissions): Self =
    withSettings(settings.andThen(_.copy(defaultMemberPermissions = Some(p))))

  def guildOnly: Self = withSettings(
    settings.andThen(_.copy(contexts = Some(List(InteractionContextType.Guild))))
  )

  def nsfw: Self = withSettings(settings.andThen(_.copy(nsfw = Some(true))))

type Completer[F[_]] = String => F[List[ApplicationCommandOptionChoice]]

/** A chat input (slash) command, or a subcommand when placed in a group. */
final case class SlashCommand[F[_]](
    name: String,
    description: String,
    options: List[ApplicationCommandOption],
    run: (CommandContext[F], List[InteractionDataOption], ResolvedData) => Either[String, F[Unit]],
    completers: Map[String, Completer[F]] = Map.empty[String, Completer[F]],
    protected val settings: ApplicationCommand => ApplicationCommand = identity
) extends Command[F]
    with CommandSettings[F, SlashCommand[F]]:

  def commandType: ApplicationCommandType = ApplicationCommandType.ChatInput

  protected def withSettings(f: ApplicationCommand => ApplicationCommand): SlashCommand[F] =
    copy(settings = f)

  /** Suggests values for a string/number option while the user types. */
  def autocomplete(option: String)(completer: Completer[F]): SlashCommand[F] =
    copy(completers = completers + (option -> completer))

  private[commands] def optionSpecs: List[ApplicationCommandOption] =
    options.map(o =>
      if completers.contains(o.name) then o.copy(autocomplete = Some(true), choices = None) else o
    )

  def spec: ApplicationCommand = settings(ApplicationCommand(name, description, options = optionSpecs))

  private[commands] def handle(ctx: CommandContext[F]): Either[String, F[Unit]] =
    run(ctx, ctx.data.options, ctx.data.resolved.getOrElse(ResolvedData()))

  private[commands] def completeWith(
      options: List[InteractionDataOption]
  ): Option[F[List[ApplicationCommandOptionChoice]]] =
    options.find(_.focused).flatMap { o =>
      val typed = o.value.map(v => v.asString.getOrElse(v.noSpaces)).getOrElse("")
      completers.get(o.name).map(_(typed))
    }

  private[commands] def complete(ctx: CommandContext[F]): Option[F[List[ApplicationCommandOptionChoice]]] =
    completeWith(ctx.data.options)

/** A named group of subcommands inside a [[GroupCommand]]. */
final case class SubcommandGroup[F[_]](name: String, description: String, subcommands: List[SlashCommand[F]])

/** A top-level command made of subcommands, e.g. `/music play` and `/music stop`. */
final case class GroupCommand[F[_]](
    name: String,
    description: String,
    children: List[SlashCommand[F] | SubcommandGroup[F]],
    protected val settings: ApplicationCommand => ApplicationCommand = identity
) extends Command[F]
    with CommandSettings[F, GroupCommand[F]]:
  import ApplicationCommandOptionType as T

  def commandType: ApplicationCommandType = ApplicationCommandType.ChatInput

  protected def withSettings(f: ApplicationCommand => ApplicationCommand): GroupCommand[F] =
    copy(settings = f)

  private def subSpec(s: SlashCommand[F]) =
    ApplicationCommandOption(T.SubCommand, s.name, s.description, options = Some(s.optionSpecs))

  def spec: ApplicationCommand =
    settings(
      ApplicationCommand(
        name,
        description,
        options = children.map {
          case s: SlashCommand[F] @unchecked    => subSpec(s)
          case g: SubcommandGroup[F] @unchecked =>
            ApplicationCommandOption(
              T.SubCommandGroup,
              g.name,
              g.description,
              options = Some(g.subcommands.map(subSpec))
            )
        }
      )
    )

  /** Finds the invoked subcommand and the options passed to it. */
  private def resolve(
      options: List[InteractionDataOption]
  ): Option[(SlashCommand[F], List[InteractionDataOption])] =
    options.headOption.flatMap { first =>
      children.collectFirstSome {
        case s: SlashCommand[F] @unchecked if first.`type` == T.SubCommand && s.name == first.name =>
          Some(s -> first.options)
        case g: SubcommandGroup[F] @unchecked if first.`type` == T.SubCommandGroup && g.name == first.name =>
          first.options.headOption.flatMap(sub =>
            g.subcommands.find(_.name == sub.name).map(_ -> sub.options)
          )
        case _ => None
      }
    }

  private[commands] def handle(ctx: CommandContext[F]): Either[String, F[Unit]] =
    resolve(ctx.data.options)
      .toRight(s"Unknown subcommand of /$name")
      .flatMap((sub, opts) => sub.run(ctx, opts, ctx.data.resolved.getOrElse(ResolvedData())))

  private[commands] def complete(ctx: CommandContext[F]): Option[F[List[ApplicationCommandOptionChoice]]] =
    resolve(ctx.data.options).flatMap((sub, opts) => sub.completeWith(opts))

/** A context menu command on users (right click > Apps). */
final case class UserCommand[F[_]](
    name: String,
    run: (CommandContext[F], User) => F[Unit],
    protected val settings: ApplicationCommand => ApplicationCommand = identity
) extends Command[F]
    with CommandSettings[F, UserCommand[F]]:
  def commandType: ApplicationCommandType = ApplicationCommandType.User
  protected def withSettings(f: ApplicationCommand => ApplicationCommand): UserCommand[F] = copy(settings = f)
  def spec: ApplicationCommand = settings(ApplicationCommand(name, `type` = commandType))
  private[commands] def handle(ctx: CommandContext[F]): Either[String, F[Unit]] =
    (ctx.data.targetId, ctx.data.resolved).tupled
      .flatMap((id, r) => r.users.get(id))
      .toRight("Could not resolve the target user")
      .map(run(ctx, _))
  private[commands] def complete(ctx: CommandContext[F]) = None

/** A context menu command on messages. */
final case class MessageCommand[F[_]](
    name: String,
    run: (CommandContext[F], Message) => F[Unit],
    protected val settings: ApplicationCommand => ApplicationCommand = identity
) extends Command[F]
    with CommandSettings[F, MessageCommand[F]]:
  def commandType: ApplicationCommandType = ApplicationCommandType.Message
  protected def withSettings(f: ApplicationCommand => ApplicationCommand): MessageCommand[F] =
    copy(settings = f)
  def spec: ApplicationCommand = settings(ApplicationCommand(name, `type` = commandType))
  private[commands] def handle(ctx: CommandContext[F]): Either[String, F[Unit]] =
    (ctx.data.targetId, ctx.data.resolved).tupled
      .flatMap((id, r) => r.messages.get(id))
      .toRight("Could not resolve the target message")
      .map(run(ctx, _))
  private[commands] def complete(ctx: CommandContext[F]) = None
