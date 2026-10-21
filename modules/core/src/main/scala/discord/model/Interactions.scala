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

package discord.model

import io.circe.{Decoder, Encoder, Json}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder}

final case class ApplicationCommandOptionChoice(name: String, value: Json) derives ConfiguredCodec

object ApplicationCommandOptionChoice:
  def string(name: String, value: String): ApplicationCommandOptionChoice =
    ApplicationCommandOptionChoice(name, Json.fromString(value))
  def int(name: String, value: Long): ApplicationCommandOptionChoice =
    ApplicationCommandOptionChoice(name, Json.fromLong(value))
  def number(name: String, value: Double): ApplicationCommandOptionChoice =
    ApplicationCommandOptionChoice(name, Json.fromDoubleOrNull(value))

final case class ApplicationCommandOption(
    `type`: ApplicationCommandOptionType,
    name: String,
    description: String,
    required: Option[Boolean] = None,
    choices: Option[List[ApplicationCommandOptionChoice]] = None,
    options: Option[List[ApplicationCommandOption]] = None,
    channelTypes: Option[List[ChannelType]] = None,
    minValue: Option[Json] = None,
    maxValue: Option[Json] = None,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    autocomplete: Option[Boolean] = None
)

object ApplicationCommandOption:
  given Encoder.AsObject[ApplicationCommandOption] = ConfiguredEncoder.derived
  given Decoder[ApplicationCommandOption] = ConfiguredDecoder.derived

/** Interaction context types: where a command can be used. */
object InteractionContextType:
  val Guild = 0
  val BotDM = 1
  val PrivateChannel = 2

/** An application command as registered with, or returned by, Discord. */
final case class ApplicationCommand(
    name: String,
    description: String = "",
    `type`: ApplicationCommandType = ApplicationCommandType.ChatInput,
    options: List[ApplicationCommandOption] = Nil,
    defaultMemberPermissions: Option[Permissions] = None,
    nsfw: Option[Boolean] = None,
    contexts: Option[List[Int]] = None,
    nameLocalizations: Option[Map[String, String]] = None,
    descriptionLocalizations: Option[Map[String, String]] = None,
    // Set by Discord.
    id: Option[Snowflake] = None,
    applicationId: Option[Snowflake] = None,
    guildId: Option[Snowflake] = None,
    version: Option[Snowflake] = None
) derives ConfiguredCodec

final case class InteractionDataOption(
    name: String,
    `type`: ApplicationCommandOptionType,
    value: Option[Json] = None,
    options: List[InteractionDataOption] = Nil,
    focused: Boolean = false
)

object InteractionDataOption:
  given Encoder.AsObject[InteractionDataOption] = ConfiguredEncoder.derived
  given Decoder[InteractionDataOption] = ConfiguredDecoder.derived

final case class ResolvedData(
    users: Map[Snowflake, User] = Map.empty,
    members: Map[Snowflake, GuildMember] = Map.empty,
    roles: Map[Snowflake, Role] = Map.empty,
    channels: Map[Snowflake, Channel] = Map.empty,
    messages: Map[Snowflake, Message] = Map.empty,
    attachments: Map[Snowflake, Attachment] = Map.empty
) derives ConfiguredCodec

/** The `data` of an interaction. Which fields are set depends on the interaction type. */
final case class InteractionData(
    // application commands
    id: Option[Snowflake] = None,
    name: Option[String] = None,
    `type`: Option[ApplicationCommandType] = None,
    resolved: Option[ResolvedData] = None,
    options: List[InteractionDataOption] = Nil,
    guildId: Option[Snowflake] = None,
    targetId: Option[Snowflake] = None,
    // message components and modals
    customId: Option[String] = None,
    componentType: Option[ComponentType] = None,
    values: List[String] = Nil,
    components: List[Component] = Nil
) derives ConfiguredCodec:
  /** The submitted modal field with this custom ID, wherever it is nested (e.g. inside a label). */
  def field(customId: String): Option[Component] =
    components.flatMap(_.flatten).find(_.customId.contains(customId))

final case class Interaction(
    id: Snowflake,
    applicationId: Snowflake,
    `type`: InteractionType,
    token: String,
    data: Option[InteractionData] = None,
    guildId: Option[Snowflake] = None,
    channel: Option[Channel] = None,
    channelId: Option[Snowflake] = None,
    member: Option[GuildMember] = None,
    user: Option[User] = None,
    version: Int = 1,
    message: Option[Message] = None,
    appPermissions: Option[Permissions] = None,
    locale: Option[String] = None,
    guildLocale: Option[String] = None
) derives ConfiguredCodec:
  /** The invoking user, whether the interaction came from a guild (via `member`) or a DM. */
  def invoker: Option[User] = member.flatMap(_.user).orElse(user)

  override def toString: String = Redact.product(this, "token")

final case class InteractionCallbackData(
    tts: Option[Boolean] = None,
    content: Option[String] = None,
    embeds: Option[List[Embed]] = None,
    allowedMentions: Option[AllowedMentions] = None,
    flags: Option[MessageFlags] = None,
    components: Option[List[Component]] = None,
    // autocomplete
    choices: Option[List[ApplicationCommandOptionChoice]] = None,
    // modal
    customId: Option[String] = None,
    title: Option[String] = None
) derives ConfiguredCodec

final case class InteractionResponse(
    `type`: InteractionCallbackType,
    data: Option[InteractionCallbackData] = None
)

object InteractionResponse:
  given Decoder[InteractionResponse] = ConfiguredDecoder.derived
  // Discord rejects explicit nulls in some callback fields, so absent values are omitted.
  given Encoder[InteractionResponse] =
    ConfiguredEncoder.derived[InteractionResponse].mapJson(_.deepDropNullValues)

  val Pong: InteractionResponse = InteractionResponse(InteractionCallbackType.Pong)

  def message(
      content: String,
      embeds: List[Embed] = Nil,
      components: List[Component] = Nil,
      ephemeral: Boolean = false
  ): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.ChannelMessageWithSource,
      Some(
        InteractionCallbackData(
          content = Some(content),
          embeds = Option.when(embeds.nonEmpty)(embeds),
          components = Option.when(components.nonEmpty)(components),
          flags = Option.when(ephemeral)(MessageFlags.Ephemeral)
        )
      )
    )

  /** A Components V2 message: no content or embeds, the components are the whole message. */
  def componentsV2(components: List[Component], ephemeral: Boolean = false): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.ChannelMessageWithSource,
      Some(InteractionCallbackData(components = Some(components), flags = Some(v2Flags(ephemeral))))
    )

  private[discord] def v2Flags(ephemeral: Boolean): MessageFlags =
    if ephemeral then MessageFlags.IsComponentsV2 | MessageFlags.Ephemeral else MessageFlags.IsComponentsV2

  def deferred(ephemeral: Boolean = false): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.DeferredChannelMessageWithSource,
      Option.when(ephemeral)(InteractionCallbackData(flags = Some(MessageFlags.Ephemeral)))
    )

  val DeferredUpdate: InteractionResponse = InteractionResponse(InteractionCallbackType.DeferredUpdateMessage)

  def autocomplete(choices: List[ApplicationCommandOptionChoice]): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.ApplicationCommandAutocompleteResult,
      Some(InteractionCallbackData(choices = Some(choices)))
    )

  def modal(customId: String, title: String, components: List[Component]): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.Modal,
      Some(
        InteractionCallbackData(customId = Some(customId), title = Some(title), components = Some(components))
      )
    )
