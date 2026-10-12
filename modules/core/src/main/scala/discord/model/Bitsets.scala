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

/** Permission bitset. Discord serializes it as a decimal string. */
opaque type Permissions = Long

object Permissions:
  def apply(bits: Long): Permissions = bits
  val None: Permissions = 0L

  val CreateInstantInvite: Permissions = 1L << 0
  val KickMembers: Permissions = 1L << 1
  val BanMembers: Permissions = 1L << 2
  val Administrator: Permissions = 1L << 3
  val ManageChannels: Permissions = 1L << 4
  val ManageGuild: Permissions = 1L << 5
  val AddReactions: Permissions = 1L << 6
  val ViewAuditLog: Permissions = 1L << 7
  val PrioritySpeaker: Permissions = 1L << 8
  val Stream: Permissions = 1L << 9
  val ViewChannel: Permissions = 1L << 10
  val SendMessages: Permissions = 1L << 11
  val SendTtsMessages: Permissions = 1L << 12
  val ManageMessages: Permissions = 1L << 13
  val EmbedLinks: Permissions = 1L << 14
  val AttachFiles: Permissions = 1L << 15
  val ReadMessageHistory: Permissions = 1L << 16
  val MentionEveryone: Permissions = 1L << 17
  val UseExternalEmojis: Permissions = 1L << 18
  val Connect: Permissions = 1L << 20
  val Speak: Permissions = 1L << 21
  val MuteMembers: Permissions = 1L << 22
  val DeafenMembers: Permissions = 1L << 23
  val MoveMembers: Permissions = 1L << 24
  val UseVad: Permissions = 1L << 25
  val ChangeNickname: Permissions = 1L << 26
  val ManageNicknames: Permissions = 1L << 27
  val ManageRoles: Permissions = 1L << 28
  val ManageWebhooks: Permissions = 1L << 29
  val ManageGuildExpressions: Permissions = 1L << 30
  val UseApplicationCommands: Permissions = 1L << 31
  val RequestToSpeak: Permissions = 1L << 32
  val ManageEvents: Permissions = 1L << 33
  val ManageThreads: Permissions = 1L << 34
  val CreatePublicThreads: Permissions = 1L << 35
  val CreatePrivateThreads: Permissions = 1L << 36
  val SendMessagesInThreads: Permissions = 1L << 38
  val ModerateMembers: Permissions = 1L << 40

  extension (p: Permissions)
    def bits: Long = p
    def |(o: Permissions): Permissions = p | o
    def &(o: Permissions): Permissions = p & o
    def has(o: Permissions): Boolean = (p & o) == o

  given Encoder[Permissions] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Permissions] =
    Decoder.decodeString.emap(s => s.toLongOption.toRight(s"Invalid permissions: $s")).or(Decoder.decodeLong)

/** Gateway intents bitset. */
opaque type Intents = Int

object Intents:
  def apply(bits: Int): Intents = bits
  val None: Intents = 0

  val Guilds: Intents = 1 << 0
  val GuildMembers: Intents = 1 << 1
  val GuildModeration: Intents = 1 << 2
  val GuildExpressions: Intents = 1 << 3
  val GuildIntegrations: Intents = 1 << 4
  val GuildWebhooks: Intents = 1 << 5
  val GuildInvites: Intents = 1 << 6
  val GuildVoiceStates: Intents = 1 << 7
  val GuildPresences: Intents = 1 << 8
  val GuildMessages: Intents = 1 << 9
  val GuildMessageReactions: Intents = 1 << 10
  val GuildMessageTyping: Intents = 1 << 11
  val DirectMessages: Intents = 1 << 12
  val DirectMessageReactions: Intents = 1 << 13
  val DirectMessageTyping: Intents = 1 << 14
  val MessageContent: Intents = 1 << 15
  val GuildScheduledEvents: Intents = 1 << 16
  val AutoModerationConfiguration: Intents = 1 << 20
  val AutoModerationExecution: Intents = 1 << 21
  val GuildMessagePolls: Intents = 1 << 24
  val DirectMessagePolls: Intents = 1 << 25

  /** Every intent that doesn't need to be enabled in the developer portal. */
  val NonPrivileged: Intents =
    Guilds | GuildModeration | GuildExpressions | GuildIntegrations | GuildWebhooks | GuildInvites |
      GuildVoiceStates | GuildMessages | GuildMessageReactions | GuildMessageTyping | DirectMessages |
      DirectMessageReactions | DirectMessageTyping | GuildScheduledEvents | AutoModerationConfiguration |
      AutoModerationExecution | GuildMessagePolls | DirectMessagePolls

  extension (i: Intents)
    def bits: Int = i
    def |(o: Intents): Intents = i | o
    def has(o: Intents): Boolean = (i & o) == o

  given Encoder[Intents] = Encoder.encodeInt
  given Decoder[Intents] = Decoder.decodeInt
