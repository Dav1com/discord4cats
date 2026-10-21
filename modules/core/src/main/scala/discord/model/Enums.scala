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

import io.circe.{Decoder, Encoder}

/** Companion helper for Discord's integer enums. They are open: values Discord adds later still decode. */
trait IntEnum[E](wrap: Int => E, unwrap: E => Int):
  given Encoder[E] = Encoder.encodeInt.contramap(unwrap)
  given Decoder[E] = Decoder.decodeInt.map(wrap)

final case class ChannelType(value: Int)
object ChannelType extends IntEnum[ChannelType](ChannelType(_), _.value):
  val GuildText = ChannelType(0)
  val DM = ChannelType(1)
  val GuildVoice = ChannelType(2)
  val GroupDM = ChannelType(3)
  val GuildCategory = ChannelType(4)
  val GuildAnnouncement = ChannelType(5)
  val AnnouncementThread = ChannelType(10)
  val PublicThread = ChannelType(11)
  val PrivateThread = ChannelType(12)
  val GuildStageVoice = ChannelType(13)
  val GuildDirectory = ChannelType(14)
  val GuildForum = ChannelType(15)
  val GuildMedia = ChannelType(16)

final case class MessageType(value: Int)
object MessageType extends IntEnum[MessageType](MessageType(_), _.value):
  val Default = MessageType(0)
  val RecipientAdd = MessageType(1)
  val RecipientRemove = MessageType(2)
  val Call = MessageType(3)
  val ChannelNameChange = MessageType(4)
  val ChannelIconChange = MessageType(5)
  val ChannelPinnedMessage = MessageType(6)
  val UserJoin = MessageType(7)
  val Reply = MessageType(19)
  val ChatInputCommand = MessageType(20)
  val ContextMenuCommand = MessageType(23)

final case class InteractionType(value: Int)
object InteractionType extends IntEnum[InteractionType](InteractionType(_), _.value):
  val Ping = InteractionType(1)
  val ApplicationCommand = InteractionType(2)
  val MessageComponent = InteractionType(3)
  val ApplicationCommandAutocomplete = InteractionType(4)
  val ModalSubmit = InteractionType(5)

final case class ApplicationCommandType(value: Int)
object ApplicationCommandType extends IntEnum[ApplicationCommandType](ApplicationCommandType(_), _.value):
  val ChatInput = ApplicationCommandType(1)
  val User = ApplicationCommandType(2)
  val Message = ApplicationCommandType(3)
  val PrimaryEntryPoint = ApplicationCommandType(4)

final case class ApplicationCommandOptionType(value: Int)
object ApplicationCommandOptionType
    extends IntEnum[ApplicationCommandOptionType](ApplicationCommandOptionType(_), _.value):
  val SubCommand = ApplicationCommandOptionType(1)
  val SubCommandGroup = ApplicationCommandOptionType(2)
  val String = ApplicationCommandOptionType(3)
  val Integer = ApplicationCommandOptionType(4)
  val Boolean = ApplicationCommandOptionType(5)
  val User = ApplicationCommandOptionType(6)
  val Channel = ApplicationCommandOptionType(7)
  val Role = ApplicationCommandOptionType(8)
  val Mentionable = ApplicationCommandOptionType(9)
  val Number = ApplicationCommandOptionType(10)
  val Attachment = ApplicationCommandOptionType(11)

final case class InteractionCallbackType(value: Int)
object InteractionCallbackType extends IntEnum[InteractionCallbackType](InteractionCallbackType(_), _.value):
  val Pong = InteractionCallbackType(1)
  val ChannelMessageWithSource = InteractionCallbackType(4)
  val DeferredChannelMessageWithSource = InteractionCallbackType(5)
  val DeferredUpdateMessage = InteractionCallbackType(6)
  val UpdateMessage = InteractionCallbackType(7)
  val ApplicationCommandAutocompleteResult = InteractionCallbackType(8)
  val Modal = InteractionCallbackType(9)

final case class ComponentType(value: Int)
object ComponentType extends IntEnum[ComponentType](ComponentType(_), _.value):
  val ActionRow = ComponentType(1)
  val Button = ComponentType(2)
  val StringSelect = ComponentType(3)
  val TextInput = ComponentType(4)
  val UserSelect = ComponentType(5)
  val RoleSelect = ComponentType(6)
  val MentionableSelect = ComponentType(7)
  val ChannelSelect = ComponentType(8)
  // Components V2 (messages need MessageFlags.IsComponentsV2)
  val Section = ComponentType(9)
  val TextDisplay = ComponentType(10)
  val Thumbnail = ComponentType(11)
  val MediaGallery = ComponentType(12)
  val File = ComponentType(13)
  val Separator = ComponentType(14)
  val Container = ComponentType(17)
  // Modals only
  val Label = ComponentType(18)
  val FileUpload = ComponentType(19)

/** Vertical padding of a [[ComponentType.Separator]]. */
final case class SeparatorSpacing(value: Int)
object SeparatorSpacing extends IntEnum[SeparatorSpacing](SeparatorSpacing(_), _.value):
  val Small = SeparatorSpacing(1)
  val Large = SeparatorSpacing(2)

/** Bit flags on a message, e.g. [[MessageFlags.Ephemeral]] for interaction replies only the invoker sees. */
final case class MessageFlags(value: Int):
  def |(o: MessageFlags): MessageFlags = MessageFlags(value | o.value)
  def has(o: MessageFlags): Boolean = (value & o.value) == o.value
object MessageFlags extends IntEnum[MessageFlags](MessageFlags(_), _.value):
  val Crossposted = MessageFlags(1 << 0)
  val SuppressEmbeds = MessageFlags(1 << 2)
  val Ephemeral = MessageFlags(1 << 6)
  val Loading = MessageFlags(1 << 7)
  val SuppressNotifications = MessageFlags(1 << 12)
  val IsVoiceMessage = MessageFlags(1 << 13)

  /** The message is laid out with Components V2: no `content` or `embeds`, only `components`. Can't be unset.
    */
  val IsComponentsV2 = MessageFlags(1 << 15)
