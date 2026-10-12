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

package discord.gateway

import discord.model.*
import discord.model.given
import io.circe.{Decoder, Json}
import io.circe.derivation.ConfiguredCodec

/** A gateway dispatch event. Event types this library doesn't model come through as [[GatewayEvent.Unknown]].
  */
enum GatewayEvent:
  case Ready(
      v: Int,
      user: User,
      guilds: List[UnavailableGuild],
      sessionId: String,
      resumeGatewayUrl: String,
      application: PartialApplication,
      shard: Option[List[Int]]
  )
  case Resumed
  case GuildCreate(guild: Guild)
  case GuildUpdate(guild: Guild)
  case GuildDelete(guild: UnavailableGuild)
  case ChannelCreate(channel: Channel)
  case ChannelUpdate(channel: Channel)
  case ChannelDelete(channel: Channel)
  case MessageCreate(message: Message)
  case MessageUpdate(message: Message)
  case MessageDelete(id: Snowflake, channelId: Snowflake, guildId: Option[Snowflake])
  case MessageReactionAdd(reaction: MessageReactionEvent)
  case MessageReactionRemove(reaction: MessageReactionEvent)
  case GuildMemberAdd(guildId: Snowflake, member: GuildMember)
  case GuildMemberUpdate(guildId: Snowflake, member: GuildMember)
  case GuildMemberRemove(guildId: Snowflake, user: User)
  case InteractionCreate(interaction: Interaction)
  case VoiceStateUpdate(state: VoiceState)
  case VoiceServerUpdate(token: String, guildId: Snowflake, endpoint: Option[String])
  case TypingStart(channelId: Snowflake, guildId: Option[Snowflake], userId: Snowflake, timestamp: Long)

  /** An event this library doesn't model, or one that failed to decode (then `error` says why). */
  case Unknown(name: String, data: Json, error: Option[String])

final case class MessageReactionEvent(
    userId: Snowflake,
    channelId: Snowflake,
    messageId: Snowflake,
    guildId: Option[Snowflake] = None,
    member: Option[GuildMember] = None,
    emoji: Emoji
) derives ConfiguredCodec

object GatewayEvent:
  private given Decoder[Ready] = Decoder.instance { c =>
    for
      v <- c.get[Int]("v")
      user <- c.get[User]("user")
      guilds <- c.getOrElse[List[UnavailableGuild]]("guilds")(Nil)
      sessionId <- c.get[String]("session_id")
      resumeUrl <- c.get[String]("resume_gateway_url")
      app <- c.get[PartialApplication]("application")
      shard <- c.get[Option[List[Int]]]("shard")
    yield Ready(v, user, guilds, sessionId, resumeUrl, app, shard)
  }

  private def memberWithGuild(d: Json): Decoder.Result[(Snowflake, GuildMember)] =
    for
      g <- d.hcursor.get[Snowflake]("guild_id")
      m <- d.as[GuildMember]
    yield (g, m)

  /** Decodes the `d` of a dispatch named `name`. */
  def decode(name: String, d: Json): GatewayEvent =
    val c = d.hcursor
    val result: Decoder.Result[GatewayEvent] = name match
      case "READY"          => d.as[Ready]
      case "RESUMED"        => Right(Resumed)
      case "GUILD_CREATE"   => d.as[Guild].map(GuildCreate(_))
      case "GUILD_UPDATE"   => d.as[Guild].map(GuildUpdate(_))
      case "GUILD_DELETE"   => d.as[UnavailableGuild].map(GuildDelete(_))
      case "CHANNEL_CREATE" => d.as[Channel].map(ChannelCreate(_))
      case "CHANNEL_UPDATE" => d.as[Channel].map(ChannelUpdate(_))
      case "CHANNEL_DELETE" => d.as[Channel].map(ChannelDelete(_))
      case "MESSAGE_CREATE" => d.as[Message].map(MessageCreate(_))
      case "MESSAGE_UPDATE" => d.as[Message].map(MessageUpdate(_))
      case "MESSAGE_DELETE" =>
        for
          id <- c.get[Snowflake]("id")
          ch <- c.get[Snowflake]("channel_id")
          g <- c.get[Option[Snowflake]]("guild_id")
        yield MessageDelete(id, ch, g)
      case "MESSAGE_REACTION_ADD"    => d.as[MessageReactionEvent].map(MessageReactionAdd(_))
      case "MESSAGE_REACTION_REMOVE" => d.as[MessageReactionEvent].map(MessageReactionRemove(_))
      case "GUILD_MEMBER_ADD"        => memberWithGuild(d).map(GuildMemberAdd(_, _))
      case "GUILD_MEMBER_UPDATE"     => memberWithGuild(d).map(GuildMemberUpdate(_, _))
      case "GUILD_MEMBER_REMOVE"     =>
        for
          g <- c.get[Snowflake]("guild_id")
          u <- c.get[User]("user")
        yield GuildMemberRemove(g, u)
      case "INTERACTION_CREATE"  => d.as[Interaction].map(InteractionCreate(_))
      case "VOICE_STATE_UPDATE"  => d.as[VoiceState].map(VoiceStateUpdate(_))
      case "VOICE_SERVER_UPDATE" =>
        for
          t <- c.get[String]("token")
          g <- c.get[Snowflake]("guild_id")
          e <- c.get[Option[String]]("endpoint")
        yield VoiceServerUpdate(t, g, e)
      case "TYPING_START" =>
        for
          ch <- c.get[Snowflake]("channel_id")
          g <- c.get[Option[Snowflake]]("guild_id")
          u <- c.get[Snowflake]("user_id")
          ts <- c.get[Long]("timestamp")
        yield TypingStart(ch, g, u, ts)
      case other => Right(Unknown(other, d, None))
    result.fold(err => Unknown(name, d, Some(err.getMessage)), identity)
