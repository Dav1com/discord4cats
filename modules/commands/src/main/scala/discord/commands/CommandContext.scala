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

import cats.effect.{Concurrent, Ref}
import cats.effect.std.Mutex
import cats.syntax.all.*
import discord.model.*
import discord.rest.{CreateMessage, DiscordRest, EditMessage}

/** What a command handler gets: the interaction plus ways to answer it.
  *
  * Discord wants a first response within 3 seconds. [[reply]] picks the right call: the initial response,
  * editing a deferred response, or a follow-up message once there already is a reply. If a handler is slow,
  * the router defers for it (see [[CommandRouter]]).
  */
final class CommandContext[F[_]] private[commands] (
    val interaction: Interaction,
    val rest: DiscordRest[F],
    state: Ref[F, CommandContext.State],
    lock: Mutex[F]
)(using F: Concurrent[F]):
  import CommandContext.State

  def user: User = interaction.invoker.getOrElse(sys.error("Interaction without a user"))
  def member: Option[GuildMember] = interaction.member
  def guildId: Option[Snowflake] = interaction.guildId
  def channelId: Option[Snowflake] = interaction.channelId.orElse(interaction.channel.map(_.id))
  def data: InteractionData = interaction.data.getOrElse(InteractionData())

  /** Whether the interaction has been answered (or deferred) yet. */
  def responded: F[Boolean] = state.get.map(_ != State.Pending)

  def reply(
      content: String,
      ephemeral: Boolean = false,
      embeds: List[Embed] = Nil,
      components: List[Component] = Nil
  ): F[Unit] =
    lock.lock.surround(state.get.flatMap {
      case State.Pending =>
        respondUnlocked(InteractionResponse.message(content, embeds, components, ephemeral))
      case State.Deferred =>
        rest
          .editOriginalInteractionResponse(
            interaction.applicationId,
            interaction.token,
            EditMessage(
              content = Optional(content),
              embeds = if embeds.isEmpty then Optional.Missing else Optional(embeds),
              components = if components.isEmpty then Optional.Missing else Optional(components)
            )
          ) >> state.set(State.Replied)
      case State.Replied =>
        rest
          .createFollowupMessage(
            interaction.applicationId,
            interaction.token,
            CreateMessage(
              content = Some(content),
              embeds = embeds,
              components = components,
              flags = Option.when(ephemeral)(MessageFlags.Ephemeral)
            )
          )
          .void
    })

  /** Like [[reply]], for a Components V2 message: the components are the whole message. */
  def replyComponents(components: List[Component], ephemeral: Boolean = false): F[Unit] =
    lock.lock.surround(state.get.flatMap {
      case State.Pending =>
        respondUnlocked(InteractionResponse.componentsV2(components, ephemeral))
      case State.Deferred =>
        rest
          .editOriginalInteractionResponse(
            interaction.applicationId,
            interaction.token,
            EditMessage(components = Optional(components), flags = Optional(MessageFlags.IsComponentsV2))
          ) >> state.set(State.Replied)
      case State.Replied =>
        rest
          .createFollowupMessage(
            interaction.applicationId,
            interaction.token,
            CreateMessage(components = components, flags = Some(InteractionResponse.v2Flags(ephemeral)))
          )
          .void
    })

  /** Acknowledges now and replies later ("Bot is thinking..."). No-op if already answered. */
  def defer(ephemeral: Boolean = false): F[Unit] =
    lock.lock.surround(state.get.flatMap {
      case State.Pending => respondUnlocked(InteractionResponse.deferred(ephemeral))
      case _             => F.unit
    })

  /** Sends a raw initial response. Fails if the interaction was already answered. */
  def respond(response: InteractionResponse): F[Unit] =
    lock.lock.surround(state.get.flatMap {
      case State.Pending => respondUnlocked(response)
      case _             =>
        F.raiseError(IllegalStateException(s"Interaction ${interaction.id.asString} was already answered"))
    })

  def showModal(customId: String, title: String, components: List[Component]): F[Unit] =
    respond(InteractionResponse.modal(customId, title, components))

  def followup(message: CreateMessage): F[Message] =
    rest.createFollowupMessage(interaction.applicationId, interaction.token, message)

  def editReply(edit: EditMessage): F[Message] =
    rest.editOriginalInteractionResponse(interaction.applicationId, interaction.token, edit)

  def deleteReply: F[Unit] =
    rest.deleteOriginalInteractionResponse(interaction.applicationId, interaction.token)

  private def respondUnlocked(response: InteractionResponse): F[Unit] =
    val next = response.`type` match
      case InteractionCallbackType.DeferredChannelMessageWithSource |
          InteractionCallbackType.DeferredUpdateMessage =>
        State.Deferred
      case _ => State.Replied
    rest.createInteractionResponse(interaction.id, interaction.token, response) >> state.set(next)

object CommandContext:
  private[commands] enum State:
    case Pending, Deferred, Replied

  def apply[F[_]: Concurrent](interaction: Interaction, rest: DiscordRest[F]): F[CommandContext[F]] =
    (Ref.of[F, State](State.Pending), Mutex[F]).mapN(new CommandContext(interaction, rest, _, _))
