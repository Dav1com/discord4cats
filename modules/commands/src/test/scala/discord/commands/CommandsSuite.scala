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

import cats.effect.{IO, Ref}
import cats.effect.testkit.TestControl
import discord.model.*
import discord.rest.DiscordRest
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.{Message as _, *}
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*

import scala.concurrent.duration.*

class CommandsSuite extends CatsEffectSuite:

  private final case class Call(method: Method, path: String, body: Json, at: FiniteDuration)

  private val messageJson = Json.obj(
    "id" := "10",
    "channel_id" := "20",
    "author" := Json.obj("id" := "30", "username" := "bot"),
    "timestamp" := "2026-01-01T00:00:00Z"
  )

  private def fakeRest: IO[(DiscordRest[IO], Ref[IO, List[Call]])] =
    for
      calls <- Ref.of[IO, List[Call]](Nil)
      app = HttpApp[IO] { req =>
        for
          body <- req.bodyText.compile.string
          now <- IO.monotonic
          _ <- calls.update(
            _ :+ Call(req.method, req.uri.path.renderString, parse(body).getOrElse(Json.Null), now)
          )
          resp <-
            if req.method == Method.PUT then Ok(Json.arr())
            else if req.method == Method.PATCH then Ok(messageJson)
            else NoContent()
        yield resp
      }
      rest <- DiscordRest[IO](Client.fromHttpApp(app), "tok")
    yield (rest, calls)

  private def interaction(tpe: Int, data: Json): Interaction =
    Json
      .obj(
        "id" := "100",
        "application_id" := "200",
        "type" := tpe,
        "token" := "itok",
        "guild_id" := "300",
        "channel_id" := "400",
        "member" := Json.obj("user" := Json.obj("id" := "500", "username" := "alice"), "roles" := Json.arr()),
        "data" := data
      )
      .as[Interaction]
      .fold(e => throw e, identity)

  private val cmds = Commands[IO]

  private val greet = cmds.slash(
    "greet",
    "Greets someone",
    Opt.user("who", "Who to greet"),
    Opt.int("times", "How often", min = Some(1)).withDefault(1)
  ) { (ctx, who, times) =>
    ctx.reply(List.fill(times.toInt)(s"Hi ${who.username}").mkString(" "))
  }

  private val music = cmds.group("music", "Music")(
    cmds.slash("play", "Play", Opt.string("file", "File"))((ctx, file) => ctx.reply(s"playing $file")),
    cmds.subgroup("queue", "Queue")(cmds.slash("clear", "Clear")(ctx => ctx.reply("cleared")))
  )

  private val slow = cmds.slash("slow", "Takes a while")(ctx => IO.sleep(5.seconds) >> ctx.reply("done"))

  private val search = cmds
    .slash("search", "Search", Opt.string("q", "Query"))((ctx, q) => ctx.reply(q))
    .autocomplete("q")(typed => IO.pure(List(ApplicationCommandOptionChoice.string(s"$typed!", typed))))

  private val callback = "/api/v10/interactions/100/itok/callback"

  test("registers the command tree in Discord's format") {
    fakeRest.flatMap { (rest, calls) =>
      CommandRouter(rest, List(greet, music)).register(Snowflake(200), Some(Snowflake(300))) >> calls.get
        .map { cs =>
          assertEquals(cs.head.method, Method.PUT)
          assertEquals(cs.head.path, "/api/v10/applications/200/guilds/300/commands")
          val expected = Json.arr(
            Json.obj(
              "name" := "greet",
              "description" := "Greets someone",
              "type" := 1,
              "options" := Json.arr(
                Json.obj("type" := 6, "name" := "who", "description" := "Who to greet", "required" := true),
                Json.obj(
                  "type" := 4,
                  "name" := "times",
                  "description" := "How often",
                  "required" := false,
                  "min_value" := 1
                )
              )
            ),
            Json.obj(
              "name" := "music",
              "description" := "Music",
              "type" := 1,
              "options" := Json.arr(
                Json.obj(
                  "type" := 1,
                  "name" := "play",
                  "description" := "Play",
                  "options" := Json.arr(
                    Json.obj("type" := 3, "name" := "file", "description" := "File", "required" := true)
                  )
                ),
                Json.obj(
                  "type" := 2,
                  "name" := "queue",
                  "description" := "Queue",
                  "options" := Json.arr(
                    Json.obj(
                      "type" := 1,
                      "name" := "clear",
                      "description" := "Clear",
                      "options" := Json.arr()
                    )
                  )
                )
              )
            )
          )
          assertEquals(cs.head.body, expected)
        }
    }
  }

  test("decodes typed options, including resolved users and defaults") {
    fakeRest.flatMap { (rest, calls) =>
      val i = interaction(
        2,
        Json.obj(
          "id" := "1",
          "name" := "greet",
          "type" := 1,
          "options" := Json.arr(Json.obj("name" := "who", "type" := 6, "value" := "600")),
          "resolved" := Json.obj("users" := Json.obj("600" := Json.obj("id" := "600", "username" := "bob")))
        )
      )
      CommandRouter(rest, List(greet)).handle(i) >> calls.get.map { cs =>
        assertEquals(cs.map(_.path), List(callback))
        assertEquals(cs.head.body, Json.obj("type" := 4, "data" := Json.obj("content" := "Hi bob")))
      }
    }
  }

  test("a missing required option gets an ephemeral error") {
    fakeRest.flatMap { (rest, calls) =>
      val i = interaction(2, Json.obj("id" := "1", "name" := "greet", "type" := 1))
      CommandRouter(rest, List(greet)).handle(i) >> calls.get.map { cs =>
        val data = cs.head.body.hcursor.downField("data")
        assertEquals(data.get[String]("content"), Right("Missing option `who`"))
        assertEquals(data.get[Int]("flags"), Right(64))
      }
    }
  }

  test("routes subcommands and subcommand groups") {
    fakeRest.flatMap { (rest, calls) =>
      val router = CommandRouter(rest, List(music))
      val play = interaction(
        2,
        Json.obj(
          "name" := "music",
          "options" := Json.arr(
            Json.obj(
              "name" := "play",
              "type" := 1,
              "options" := Json.arr(Json.obj("name" := "file", "type" := 3, "value" := "a.opus"))
            )
          )
        )
      )
      val clear = interaction(
        2,
        Json.obj(
          "name" := "music",
          "options" := Json.arr(
            Json.obj(
              "name" := "queue",
              "type" := 2,
              "options" := Json.arr(Json.obj("name" := "clear", "type" := 1))
            )
          )
        )
      )
      router.handle(play) >> router.handle(clear) >> calls.get.map { cs =>
        assertEquals(
          cs.map(_.body.hcursor.downField("data").get[String]("content").toOption),
          List(Some("playing a.opus"), Some("cleared"))
        )
      }
    }
  }

  test("slow handlers are deferred automatically, then their reply edits the original response") {
    val program = fakeRest.flatMap { (rest, calls) =>
      CommandRouter(rest, List(slow)).handle(interaction(2, Json.obj("name" := "slow"))) >> calls.get
    }
    TestControl.executeEmbed(program).map { cs =>
      assertEquals(
        cs.map(c => (c.method, c.path)),
        List(
          Method.POST -> callback,
          Method.PATCH -> "/api/v10/webhooks/200/itok/messages/@original"
        )
      )
      assertEquals(cs(0).body, Json.obj("type" := 5))
      assertEquals(cs(0).at, 2500.millis)
      assertEquals(cs(1).body, Json.obj("content" := "done"))
    }
  }

  test("answers autocomplete requests for focused options") {
    fakeRest.flatMap { (rest, calls) =>
      val i = interaction(
        4,
        Json.obj(
          "name" := "search",
          "options" := Json.arr(Json.obj("name" := "q", "type" := 3, "value" := "ab", "focused" := true))
        )
      )
      CommandRouter(rest, List(search)).handle(i) >> calls.get.map { cs =>
        assertEquals(
          cs.head.body,
          Json.obj(
            "type" := 8,
            "data" := Json.obj("choices" := Json.arr(Json.obj("name" := "ab!", "value" := "ab")))
          )
        )
      }
    }
  }
