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

package discord.rest

import discord.json.JsonObj
import discord.json.JsonObj.*
import discord.model.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import scodec.bits.ByteVector

/** A file to upload with a message. */
final case class FileUpload(
    filename: String,
    content: ByteVector,
    contentType: String = "application/octet-stream",
    description: Option[String] = None
)

final case class CreateMessage(
    content: Option[String] = None,
    embeds: List[Embed] = Nil,
    components: List[Component] = Nil,
    tts: Boolean = false,
    allowedMentions: Option[AllowedMentions] = None,
    messageReference: Option[MessageReference] = None,
    flags: Option[MessageFlags] = None,
    files: List[FileUpload] = Nil
):
  def replyTo(message: Message): CreateMessage =
    copy(messageReference =
      Some(MessageReference(messageId = Some(message.id), channelId = Some(message.channelId)))
    )

object CreateMessage:
  def text(content: String): CreateMessage = CreateMessage(content = Some(content))

  /** A Components V2 message. Upload files with `files` and reference them as `attachment://<filename>`. */
  def componentsV2(components: Component*): CreateMessage =
    CreateMessage(components = components.toList, flags = Some(MessageFlags.IsComponentsV2))

  given Encoder[CreateMessage] = Encoder.instance { m =>
    JsonObj(
      "content" :=? m.content,
      "embeds" :=? Option.when(m.embeds.nonEmpty)(m.embeds),
      "components" :=? Option.when(m.components.nonEmpty)(m.components),
      "tts" :=? Option.when(m.tts)(true),
      "allowed_mentions" :=? m.allowedMentions,
      "message_reference" :=? m.messageReference,
      "flags" :=? m.flags,
      "attachments" :=? Option.when(m.files.nonEmpty)(attachmentsJson(m.files))
    )
  }

  private[rest] def attachmentsJson(files: List[FileUpload]): List[Json] =
    files.zipWithIndex.map { (f, i) =>
      JsonObj("id" := i, "filename" := f.filename, "description" :=? f.description)
    }

/** Edits a message. `Missing` fields are left as they are; `Null` clears them. */
final case class EditMessage(
    content: Optional[String] = Optional.Missing,
    embeds: Optional[List[Embed]] = Optional.Missing,
    components: Optional[List[Component]] = Optional.Missing,
    flags: Optional[MessageFlags] = Optional.Missing,
    allowedMentions: Optional[AllowedMentions] = Optional.Missing,
    files: List[FileUpload] = Nil
)

object EditMessage:
  def content(content: String): EditMessage = EditMessage(content = Optional(content))

  given Encoder[EditMessage] = Encoder.instance { m =>
    JsonObj(
      "content" :=! m.content,
      "embeds" :=! m.embeds,
      "components" :=! m.components,
      "flags" :=! m.flags,
      "allowed_mentions" :=! m.allowedMentions,
      "attachments" :=? Option.when(m.files.nonEmpty)(CreateMessage.attachmentsJson(m.files))
    )
  }

final case class ModifyChannel(
    name: Optional[String] = Optional.Missing,
    topic: Optional[String] = Optional.Missing,
    nsfw: Optional[Boolean] = Optional.Missing,
    position: Optional[Int] = Optional.Missing,
    rateLimitPerUser: Optional[Int] = Optional.Missing,
    bitrate: Optional[Int] = Optional.Missing,
    userLimit: Optional[Int] = Optional.Missing,
    parentId: Optional[Snowflake] = Optional.Missing,
    rtcRegion: Optional[String] = Optional.Missing
)

object ModifyChannel:
  given Encoder[ModifyChannel] = Encoder.instance { c =>
    JsonObj(
      "name" :=! c.name,
      "topic" :=! c.topic,
      "nsfw" :=! c.nsfw,
      "position" :=! c.position,
      "rate_limit_per_user" :=! c.rateLimitPerUser,
      "bitrate" :=! c.bitrate,
      "user_limit" :=! c.userLimit,
      "parent_id" :=! c.parentId,
      "rtc_region" :=! c.rtcRegion
    )
  }
