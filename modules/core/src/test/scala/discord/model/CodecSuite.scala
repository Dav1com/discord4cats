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

import discord.json.JsonObj
import discord.json.JsonObj.*
import io.circe.{Decoder, Json}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class CodecSuite extends FunSuite:

  test("snowflakes encode as strings, decode from strings or numbers, and carry a timestamp") {
    val id = Snowflake(175928847299117063L)
    assertEquals(id.asJson, Json.fromString("175928847299117063"))
    assertEquals(decode[Snowflake]("\"175928847299117063\""), Right(id))
    // Numbers are accepted too. (On Scala.js, JSON.parse turns numbers beyond 2^53 into doubles, which is why
    // Discord sends IDs as strings.)
    assertEquals(decode[Snowflake]("1234"), Right(Snowflake(1234L)))
    // Example from Discord's docs: 2016-04-30T11:18:25.796Z
    assertEquals(id.timestampMillis, 1462015105796L)
    assert(decode[Snowflake]("\"nope\"").isLeft)
  }

  test("permissions round-trip through their string form") {
    val p = Permissions.SendMessages | Permissions.Connect | Permissions.Speak
    assertEquals(decode[Permissions](p.asJson.noSpaces), Right(p))
    assert(p.has(Permissions.Connect))
    assert(!p.has(Permissions.Administrator))
  }

  test("integer enums keep values the library doesn't know") {
    assertEquals(decode[ChannelType]("2"), Right(ChannelType.GuildVoice))
    assertEquals(decode[ChannelType]("99"), Right(ChannelType(99)))
    assertEquals(ChannelType(99).asJson, Json.fromInt(99))
  }

  test("Optional distinguishes missing, null and present") {
    final case class Patch(nick: Optional[String])
    given Decoder[Patch] = Decoder.instance(c => c.downField("nick").as[Optional[String]].map(Patch(_)))
    assertEquals(decode[Patch]("{}"), Right(Patch(Optional.Missing)))
    assertEquals(decode[Patch]("""{"nick":null}"""), Right(Patch(Optional.Null)))
    assertEquals(decode[Patch]("""{"nick":"x"}"""), Right(Patch(Optional("x"))))
  }

  test("JsonObj omits absent fields and keeps explicit nulls") {
    val json = JsonObj(
      "a" := 1,
      "b" :=? Option.empty[Int],
      "c" :=? Some("x"),
      "d" :=! (Optional.Null: Optional[String]),
      "e" :=! (Optional.Missing: Optional[String])
    )
    assertEquals(json, Json.obj("a" := 1, "c" := "x", "d" := Json.Null))
  }

  test("decodes a MESSAGE_CREATE payload") {
    val json = """{
      "id": "1100000000000000001", "channel_id": "1100000000000000002", "guild_id": "1100000000000000003",
      "author": {"id": "1100000000000000004", "username": "alice", "discriminator": "0", "global_name": "Alice",
                 "avatar": null},
      "member": {"roles": ["1100000000000000005"], "joined_at": "2024-01-01T00:00:00+00:00", "deaf": false,
                 "mute": false},
      "content": "!ping", "timestamp": "2026-10-12T00:00:00.000000+00:00", "edited_timestamp": null,
      "tts": false, "mention_everyone": false, "mentions": [], "mention_roles": [], "attachments": [],
      "embeds": [{"title": "t", "fields": [{"name": "n", "value": "v", "inline": true}]}],
      "pinned": false, "type": 0, "flags": 0, "components": [], "some_future_field": {"x": 1}
    }"""
    val m = decode[Message](json).fold(e => fail(e.toString), identity)
    assertEquals(m.content, "!ping")
    assertEquals(m.author.displayName, "Alice")
    assertEquals(m.guildId, Some(Snowflake(1100000000000000003L)))
    assertEquals(m.member.map(_.roles), Some(List(Snowflake(1100000000000000005L))))
    assertEquals(m.embeds.head.fields.head, EmbedField("n", "v", inline = true))
  }

  test("decodes a slash command INTERACTION_CREATE payload with resolved data") {
    val json = """{
      "id": "1200000000000000001", "application_id": "1200000000000000002", "type": 2, "token": "tok",
      "version": 1, "guild_id": "1200000000000000003", "channel_id": "1200000000000000004",
      "member": {"user": {"id": "1200000000000000005", "username": "bob"}, "roles": [],
                 "permissions": "2147483647", "deaf": false, "mute": false},
      "data": {
        "id": "1200000000000000006", "name": "music", "type": 1,
        "options": [{"name": "play", "type": 1, "options": [
          {"name": "file", "type": 3, "value": "song.opus"},
          {"name": "where", "type": 7, "value": "1200000000000000007"}
        ]}],
        "resolved": {"channels": {"1200000000000000007": {"id": "1200000000000000007", "type": 2,
                                                         "name": "Voice", "permissions": "0"}}}
      }
    }"""
    val i = decode[Interaction](json).fold(e => fail(e.toString), identity)
    assertEquals(i.`type`, InteractionType.ApplicationCommand)
    assertEquals(i.invoker.map(_.username), Some("bob"))
    val sub = i.data.get.options.head
    assertEquals(sub.`type`, ApplicationCommandOptionType.SubCommand)
    assertEquals(sub.options.map(_.name), List("file", "where"))
    assertEquals(
      i.data.get.resolved.get.channels(Snowflake(1200000000000000007L)).`type`,
      ChannelType.GuildVoice
    )
  }

  test("interaction responses encode Discord's shape") {
    val json = InteractionResponse.message("hi", ephemeral = true).asJson
    assertEquals(json, Json.obj("type" := 4, "data" := Json.obj("content" := "hi", "flags" := 64)))
  }

  test("secrets are redacted from toString and logged JSON") {
    val i = Interaction(Snowflake(1), Snowflake(2), InteractionType(2), "itok-secret")
    assert(!i.toString.contains("itok-secret"), i.toString)
    assert(i.toString.startsWith("Interaction(1, 2,"), i.toString)
    val j = Json.obj("token" := "t", "nested" := Json.arr(Json.obj("secret_key" := List(1, 2))), "ok" := 1)
    assertEquals(
      Redact.json(j),
      Json.obj(
        "token" := "<redacted>",
        "nested" := Json.arr(Json.obj("secret_key" := "<redacted>")),
        "ok" := 1
      )
    )
  }

  test("Components V2 messages encode Discord's shape, omitting fields that don't apply") {
    val c = Component
      .container(
        Component.section(Component.thumbnail("https://x/a.png"), Component.textDisplay("# Hi")),
        Component.separator(spacing = SeparatorSpacing.Large),
        Component.mediaGallery(MediaGalleryItem("attachment://b.png")),
        Component.actionRow(Component.button("ok", "OK"))
      )
      .copy(accentColor = Some(0xff0000))
    val json = InteractionResponse.componentsV2(List(c), ephemeral = true).asJson
    val expected = Json.obj(
      "type" := 4,
      "data" := Json.obj(
        "flags" := (1 << 15 | 1 << 6),
        "components" := List(
          Json.obj(
            "type" := 17,
            "components" := List(
              Json.obj(
                "type" := 9,
                "components" := List(Json.obj("type" := 10, "content" := "# Hi")),
                "accessory" := Json.obj("type" := 11, "media" := Json.obj("url" := "https://x/a.png"))
              ),
              Json.obj("type" := 14, "divider" := true, "spacing" := 2),
              Json.obj(
                "type" := 12,
                "items" := List(Json.obj("media" := Json.obj("url" := "attachment://b.png")))
              ),
              Json.obj(
                "type" := 1,
                "components" := List(
                  Json.obj("type" := 2, "custom_id" := "ok", "style" := 1, "label" := "OK")
                )
              )
            ),
            "accent_color" := 0xff0000
          )
        )
      )
    )
    assertEquals(json, expected)
    assertEquals(decode[Component](c.asJson.noSpaces), Right(c))
  }

  test("decodes a modal submission with labelled fields") {
    val json = """{
      "id": "1300000000000000001", "application_id": "1300000000000000002", "type": 5, "token": "tok",
      "user": {"id": "1300000000000000003", "username": "carol"},
      "data": {"custom_id": "feedback", "components": [
        {"type": 18, "id": 1, "component": {"type": 4, "id": 2, "custom_id": "text", "value": "great"}},
        {"type": 18, "id": 3, "component": {"type": 3, "id": 4, "custom_id": "rating", "values": ["5"]}},
        {"type": 10, "id": 5}
      ]}
    }"""
    val i = decode[Interaction](json).fold(e => fail(e.toString), identity)
    val data = i.data.get
    assertEquals(data.field("text").flatMap(_.value), Some("great"))
    assertEquals(data.field("rating").map(_.values), Some(List("5")))
    assertEquals(data.components.head.component.map(_.`type`), Some(ComponentType.TextInput))
  }
