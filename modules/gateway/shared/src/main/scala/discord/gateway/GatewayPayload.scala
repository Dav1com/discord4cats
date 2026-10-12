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

import discord.json.JsonObj
import discord.json.JsonObj.*
import discord.model.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** The envelope of every gateway message. */
final case class GatewayPayload(
    op: Int,
    d: Json = Json.Null,
    s: Option[Long] = None,
    t: Option[String] = None
)

object GatewayPayload:
  given Decoder[GatewayPayload] = Decoder.instance { c =>
    for
      op <- c.get[Int]("op")
      d <- c.getOrElse[Json]("d")(Json.Null)
      s <- c.get[Option[Long]]("s")
      t <- c.get[Option[String]]("t")
    yield GatewayPayload(op, d, s, t)
  }
  given Encoder[GatewayPayload] = Encoder.instance(p => Json.obj("op" := p.op, "d" := p.d))

object Opcode:
  val Dispatch = 0
  val Heartbeat = 1
  val Identify = 2
  val PresenceUpdate = 3
  val VoiceStateUpdate = 4
  val Resume = 6
  val Reconnect = 7
  val RequestGuildMembers = 8
  val InvalidSession = 9
  val Hello = 10
  val HeartbeatAck = 11

final case class Activity(
    name: String,
    `type`: Int = 0,
    url: Option[String] = None,
    state: Option[String] = None
)
object Activity:
  given Encoder[Activity] =
    Encoder.instance(a => JsonObj("name" := a.name, "type" := a.`type`, "url" :=? a.url, "state" :=? a.state))
  def playing(name: String): Activity = Activity(name, 0)
  def listening(name: String): Activity = Activity(name, 2)
  def watching(name: String): Activity = Activity(name, 3)
  def custom(state: String): Activity = Activity("Custom Status", 4, state = Some(state))

/** Presence shown for the bot. `status` is one of `online`, `dnd`, `idle`, `invisible`. */
final case class Presence(activities: List[Activity] = Nil, status: String = "online", afk: Boolean = false)
object Presence:
  given Encoder[Presence] = Encoder.instance(p =>
    Json.obj("since" := Json.Null, "activities" := p.activities, "status" := p.status, "afk" := p.afk)
  )

/** Commands a client can send over the gateway. */
enum GatewayCommand:
  case UpdatePresence(presence: Presence)
  case UpdateVoiceState(
      guildId: Snowflake,
      channelId: Option[Snowflake],
      selfMute: Boolean,
      selfDeaf: Boolean
  )
  case RequestGuildMembers(
      guildId: Snowflake,
      query: Option[String] = Some(""),
      limit: Int = 0,
      userIds: List[Snowflake] = Nil,
      presences: Boolean = false,
      nonce: Option[String] = None
  )

  def toPayload: GatewayPayload = this match
    case UpdatePresence(p)                  => GatewayPayload(Opcode.PresenceUpdate, p.asJson)
    case UpdateVoiceState(g, c, mute, deaf) =>
      GatewayPayload(
        Opcode.VoiceStateUpdate,
        Json.obj("guild_id" := g, "channel_id" := c, "self_mute" := mute, "self_deaf" := deaf)
      )
    case RequestGuildMembers(g, q, limit, ids, presences, nonce) =>
      GatewayPayload(
        Opcode.RequestGuildMembers,
        JsonObj(
          "guild_id" := g,
          "query" :=? (if ids.isEmpty then q else None),
          "limit" := limit,
          "user_ids" :=? Option.when(ids.nonEmpty)(ids),
          "presences" := presences,
          "nonce" :=? nonce
        )
      )
