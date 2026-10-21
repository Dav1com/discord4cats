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

final case class User(
    id: Snowflake,
    username: String,
    discriminator: String = "0",
    globalName: Option[String] = None,
    avatar: Option[String] = None,
    bot: Boolean = false,
    system: Boolean = false
) derives ConfiguredCodec:
  def displayName: String = globalName.getOrElse(username)
  def mention: String = s"<@${id.asString}>"

final case class Role(
    id: Snowflake,
    name: String,
    color: Int = 0,
    hoist: Boolean = false,
    position: Int = 0,
    permissions: Permissions = Permissions.None,
    managed: Boolean = false,
    mentionable: Boolean = false
) derives ConfiguredCodec

final case class Emoji(
    id: Option[Snowflake] = None,
    name: Option[String] = None,
    animated: Boolean = false
) derives ConfiguredCodec

final case class GuildMember(
    user: Option[User] = None,
    nick: Option[String] = None,
    avatar: Option[String] = None,
    roles: List[Snowflake] = Nil,
    joinedAt: Option[String] = None,
    deaf: Boolean = false,
    mute: Boolean = false,
    pending: Boolean = false,
    permissions: Option[Permissions] = None,
    communicationDisabledUntil: Option[String] = None
) derives ConfiguredCodec

final case class PermissionOverwrite(id: Snowflake, `type`: Int, allow: Permissions, deny: Permissions)
    derives ConfiguredCodec

final case class Channel(
    id: Snowflake,
    `type`: ChannelType,
    guildId: Option[Snowflake] = None,
    position: Option[Int] = None,
    permissionOverwrites: List[PermissionOverwrite] = Nil,
    name: Option[String] = None,
    topic: Option[String] = None,
    nsfw: Boolean = false,
    lastMessageId: Option[Snowflake] = None,
    bitrate: Option[Int] = None,
    userLimit: Option[Int] = None,
    rateLimitPerUser: Option[Int] = None,
    recipients: List[User] = Nil,
    parentId: Option[Snowflake] = None,
    rtcRegion: Option[String] = None,
    permissions: Option[Permissions] = None
) derives ConfiguredCodec:
  def mention: String = s"<#${id.asString}>"

final case class Attachment(
    id: Snowflake,
    filename: String,
    size: Long,
    url: String,
    proxyUrl: String,
    contentType: Option[String] = None,
    description: Option[String] = None,
    height: Option[Int] = None,
    width: Option[Int] = None,
    ephemeral: Boolean = false
) derives ConfiguredCodec

final case class EmbedFooter(text: String, iconUrl: Option[String] = None) derives ConfiguredCodec
final case class EmbedMedia(url: String, height: Option[Int] = None, width: Option[Int] = None)
    derives ConfiguredCodec
final case class EmbedAuthor(name: String, url: Option[String] = None, iconUrl: Option[String] = None)
    derives ConfiguredCodec
final case class EmbedField(name: String, value: String, inline: Boolean = false) derives ConfiguredCodec

final case class Embed(
    title: Option[String] = None,
    description: Option[String] = None,
    url: Option[String] = None,
    timestamp: Option[String] = None,
    color: Option[Int] = None,
    footer: Option[EmbedFooter] = None,
    image: Option[EmbedMedia] = None,
    thumbnail: Option[EmbedMedia] = None,
    author: Option[EmbedAuthor] = None,
    fields: List[EmbedField] = Nil
) derives ConfiguredCodec

final case class Reaction(count: Int, me: Boolean, emoji: Emoji) derives ConfiguredCodec

final case class MessageReference(
    messageId: Option[Snowflake] = None,
    channelId: Option[Snowflake] = None,
    guildId: Option[Snowflake] = None,
    failIfNotExists: Option[Boolean] = None
) derives ConfiguredCodec

final case class AllowedMentions(
    parse: List[String] = Nil,
    roles: List[Snowflake] = Nil,
    users: List[Snowflake] = Nil,
    repliedUser: Boolean = false
) derives ConfiguredCodec
object AllowedMentions:
  val None: AllowedMentions = AllowedMentions()
  val All: AllowedMentions = AllowedMentions(parse = List("roles", "users", "everyone"))

final case class SelectOption(
    label: String,
    value: String,
    description: Option[String] = None,
    emoji: Option[Emoji] = None,
    default: Option[Boolean] = None
) derives ConfiguredCodec

/** Media referenced by a V2 component: any URL, or `attachment://<filename>` for a file uploaded with the
  * message. The other fields are filled in by Discord.
  */
final case class UnfurledMediaItem(
    url: String,
    proxyUrl: Option[String] = None,
    height: Option[Int] = None,
    width: Option[Int] = None,
    contentType: Option[String] = None,
    attachmentId: Option[Snowflake] = None
) derives ConfiguredCodec

final case class MediaGalleryItem(
    media: UnfurledMediaItem,
    description: Option[String] = None,
    spoiler: Option[Boolean] = None
) derives ConfiguredCodec

object MediaGalleryItem:
  def apply(url: String): MediaGalleryItem = MediaGalleryItem(UnfurledMediaItem(url))

/** A message or modal component: action row, button, select menu, text input, or one of the Components V2
  * layout and content types (section, text display, thumbnail, media gallery, file, separator, container,
  * label, file upload). One flat shape covers every kind; see the builders in the companion.
  */
final case class Component(
    `type`: ComponentType,
    id: Option[Int] = None,
    customId: Option[String] = None,
    style: Option[Int] = None,
    label: Option[String] = None,
    emoji: Option[Emoji] = None,
    url: Option[String] = None,
    disabled: Option[Boolean] = None,
    components: List[Component] = Nil,
    options: List[SelectOption] = Nil,
    placeholder: Option[String] = None,
    minValues: Option[Int] = None,
    maxValues: Option[Int] = None,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    required: Option[Boolean] = None,
    value: Option[String] = None,
    // modal submissions of selects and file uploads
    values: List[String] = Nil,
    // Components V2
    content: Option[String] = None,
    accessory: Option[Component] = None,
    component: Option[Component] = None,
    media: Option[UnfurledMediaItem] = None,
    items: List[MediaGalleryItem] = Nil,
    file: Option[UnfurledMediaItem] = None,
    description: Option[String] = None,
    spoiler: Option[Boolean] = None,
    divider: Option[Boolean] = None,
    spacing: Option[SeparatorSpacing] = None,
    accentColor: Option[Int] = None,
    name: Option[String] = None,
    size: Option[Long] = None
):
  /** This component and everything nested in it (children, accessory, labelled component), depth-first. */
  def flatten: List[Component] =
    this :: (components ++ accessory ++ component).flatMap(_.flatten)

object Component:
  given Decoder[Component] = ConfiguredDecoder.derived
  // Every kind shares this shape, so fields that don't apply are dropped rather than sent as null or [].
  given Encoder.AsObject[Component] = ConfiguredEncoder
    .derived[Component]
    .mapJsonObject(_.filter((_, v) => !v.isNull && !v.asArray.exists(_.isEmpty)))

  def actionRow(components: Component*): Component =
    Component(ComponentType.ActionRow, components = components.toList)
  def button(customId: String, label: String, style: Int = 1): Component =
    Component(ComponentType.Button, customId = Some(customId), label = Some(label), style = Some(style))
  def linkButton(url: String, label: String): Component =
    Component(ComponentType.Button, url = Some(url), label = Some(label), style = Some(5))
  def stringSelect(customId: String, options: SelectOption*): Component =
    Component(ComponentType.StringSelect, customId = Some(customId), options = options.toList)

  /** A text input for a modal. `style` is 1 for a single line, 2 for a paragraph. Wrap it in a [[label]]. */
  def textInput(customId: String, style: Int = 1): Component =
    Component(ComponentType.TextInput, customId = Some(customId), style = Some(style))

  /** Up to three text displays, with a button or thumbnail beside them. */
  def section(accessory: Component, texts: Component*): Component =
    Component(ComponentType.Section, components = texts.toList, accessory = Some(accessory))

  /** Markdown text. */
  def textDisplay(content: String): Component =
    Component(ComponentType.TextDisplay, content = Some(content))

  /** A small image, used as a section accessory. */
  def thumbnail(url: String, description: Option[String] = None, spoiler: Boolean = false): Component =
    Component(
      ComponentType.Thumbnail,
      media = Some(UnfurledMediaItem(url)),
      description = description,
      spoiler = Option.when(spoiler)(true)
    )

  def mediaGallery(items: MediaGalleryItem*): Component =
    Component(ComponentType.MediaGallery, items = items.toList)

  /** An uploaded file, referenced as `attachment://<filename>`. */
  def file(url: String, spoiler: Boolean = false): Component =
    Component(ComponentType.File, file = Some(UnfurledMediaItem(url)), spoiler = Option.when(spoiler)(true))

  def separator(divider: Boolean = true, spacing: SeparatorSpacing = SeparatorSpacing.Small): Component =
    Component(ComponentType.Separator, divider = Some(divider), spacing = Some(spacing))

  /** Groups components in a box, like an embed. For a coloured left border, `.copy(accentColor = Some(rgb))`.
    */
  def container(components: Component*): Component =
    Component(ComponentType.Container, components = components.toList)

  /** A modal field: a title and optional description around one text input, select or file upload. */
  def label(label: String, component: Component, description: Option[String] = None): Component =
    Component(
      ComponentType.Label,
      label = Some(label),
      component = Some(component),
      description = description
    )

  /** Lets the user attach files in a modal. Wrap it in a [[label]]. */
  def fileUpload(customId: String, minValues: Int = 1, maxValues: Int = 1): Component =
    Component(
      ComponentType.FileUpload,
      customId = Some(customId),
      minValues = Some(minValues),
      maxValues = Some(maxValues)
    )

final case class Message(
    id: Snowflake,
    channelId: Snowflake,
    author: User,
    content: String = "",
    timestamp: String,
    editedTimestamp: Option[String] = None,
    tts: Boolean = false,
    mentionEveryone: Boolean = false,
    mentions: List[User] = Nil,
    mentionRoles: List[Snowflake] = Nil,
    attachments: List[Attachment] = Nil,
    embeds: List[Embed] = Nil,
    reactions: List[Reaction] = Nil,
    pinned: Boolean = false,
    webhookId: Option[Snowflake] = None,
    `type`: MessageType = MessageType.Default,
    flags: Option[MessageFlags] = None,
    messageReference: Option[MessageReference] = None,
    components: List[Component] = Nil,
    // Only present on gateway MESSAGE_CREATE / MESSAGE_UPDATE events.
    guildId: Option[Snowflake] = None,
    member: Option[GuildMember] = None
) derives ConfiguredCodec

final case class UnavailableGuild(id: Snowflake, unavailable: Boolean = true) derives ConfiguredCodec

final case class VoiceState(
    guildId: Option[Snowflake] = None,
    channelId: Option[Snowflake],
    userId: Snowflake,
    member: Option[GuildMember] = None,
    sessionId: String,
    deaf: Boolean = false,
    mute: Boolean = false,
    selfDeaf: Boolean = false,
    selfMute: Boolean = false,
    selfStream: Boolean = false,
    selfVideo: Boolean = false,
    suppress: Boolean = false
) derives ConfiguredCodec

final case class Guild(
    id: Snowflake,
    name: String,
    icon: Option[String] = None,
    ownerId: Option[Snowflake] = None,
    permissions: Option[Permissions] = None,
    afkChannelId: Option[Snowflake] = None,
    roles: List[Role] = Nil,
    emojis: List[Emoji] = Nil,
    features: List[String] = Nil,
    preferredLocale: Option[String] = None,
    // The fields below are only sent in the gateway GUILD_CREATE event.
    joinedAt: Option[String] = None,
    large: Option[Boolean] = None,
    memberCount: Option[Int] = None,
    members: List[GuildMember] = Nil,
    channels: List[Channel] = Nil,
    threads: List[Channel] = Nil,
    voiceStates: List[VoiceState] = Nil
) derives ConfiguredCodec

final case class Application(
    id: Snowflake,
    name: String,
    description: String = "",
    botPublic: Boolean = false,
    owner: Option[User] = None,
    flags: Option[Int] = None
) derives ConfiguredCodec

/** Partial application object sent in READY. */
final case class PartialApplication(id: Snowflake, flags: Option[Int] = None) derives ConfiguredCodec

final case class SessionStartLimit(total: Int, remaining: Int, resetAfter: Long, maxConcurrency: Int)
    derives ConfiguredCodec

final case class GatewayBot(url: String, shards: Int, sessionStartLimit: SessionStartLimit)
    derives ConfiguredCodec

/** Opaque JSON kept for fields this library doesn't model yet. */
type RawJson = Json
