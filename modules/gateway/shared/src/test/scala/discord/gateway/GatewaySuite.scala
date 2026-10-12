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

import cats.effect.IO
import cats.effect.testkit.TestControl
import discord.model.*
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class GatewaySuite extends CatsEffectSuite:

  private val config = GatewayConfig("tok", Intents.Guilds | Intents.GuildMessages)

  private def hello(interval: Long = 45000) =
    Json.obj("op" := 10, "d" := Json.obj("heartbeat_interval" := interval))

  private def dispatch(name: String, seq: Long, d: Json) =
    Json.obj("op" := 0, "t" := name, "s" := seq, "d" := d)

  private val readyData = Json.obj(
    "v" := 10,
    "user" := Json.obj("id" := "1", "username" := "bot", "bot" := true),
    "guilds" := List(Json.obj("id" := "2", "unavailable" := true)),
    "session_id" := "sess",
    "resume_gateway_url" := "wss://resume.example",
    "application" := Json.obj("id" := "3")
  )

  test("identifies, publishes READY, then resumes on the resume URL after op 7") {
    val program = FakeWs().flatMap { ws =>
      Gateway.connect(ws, "wss://gateway.example", config).use { gw =>
        gw.subscribe.use { events =>
          for
            c1 <- ws.accept
            _ <- c1.push(hello())
            identify <- c1.nextOp(Opcode.Identify)
            _ <- c1.push(dispatch("READY", 1, readyData))
            _ <- c1.push(dispatch("SOME_FUTURE_EVENT", 2, Json.obj("x" := 1)))
            first2 <- events.take(2).compile.toList
            _ <- c1.push(Json.obj("op" := 7, "d" := Json.Null))
            c2 <- ws.accept
            _ <- c2.push(hello())
            resume <- c2.nextOp(Opcode.Resume)
          yield (c1.uri.renderString, identify, first2, c2.uri.renderString, resume)
        }
      }
    }
    TestControl.executeEmbed(program).map { (uri1, identify, first2, uri2, resume) =>
      assertEquals(uri1, "wss://gateway.example?v=10&encoding=json")
      val d = identify.hcursor.downField("d")
      assertEquals(d.get[String]("token"), Right("tok"))
      assertEquals(d.get[Int]("intents"), Right((Intents.Guilds | Intents.GuildMessages).bits))
      first2 match
        case List(r: GatewayEvent.Ready, GatewayEvent.Unknown("SOME_FUTURE_EVENT", _, None)) =>
          assertEquals(r.sessionId, "sess")
        case other => fail(s"Unexpected events: $other")
      assertEquals(uri2, "wss://resume.example?v=10&encoding=json")
      assertEquals(
        resume.hcursor.downField("d").as[Json],
        Right(Json.obj("token" := "tok", "session_id" := "sess", "seq" := 2))
      )
    }
  }

  test("sends heartbeats with the last sequence number") {
    val program = FakeWs().flatMap { ws =>
      Gateway.connect(ws, "wss://gateway.example", config).use { _ =>
        for
          c <- ws.accept
          _ <- c.push(hello(1000))
          _ <- c.nextOp(Opcode.Identify)
          _ <- c.push(dispatch("READY", 5, readyData))
          beat1 <- c.nextOp(Opcode.Heartbeat)
          _ <- c.push(Json.obj("op" := 11))
          t1 <- IO.monotonic
          beat2 <- c.nextOp(Opcode.Heartbeat)
          t2 <- IO.monotonic
        yield (beat1, beat2, t2 - t1)
      }
    }
    TestControl.executeEmbed(program).map { (b1, b2, gap) =>
      assertEquals(b1.hcursor.get[Long]("d"), Right(5L))
      assertEquals(b2.hcursor.get[Long]("d"), Right(5L))
      assertEquals(gap, 1.second)
    }
  }

  test("reconnects with a fresh identify when heartbeats aren't acknowledged") {
    val program = FakeWs().flatMap { ws =>
      Gateway.connect(ws, "wss://gateway.example", config).use { _ =>
        for
          c1 <- ws.accept
          _ <- c1.push(hello(1000))
          _ <- c1.nextOp(Opcode.Identify)
          _ <- c1.nextOp(Opcode.Heartbeat) // never ACKed
          c2 <- ws.accept
          _ <- c2.push(hello(1000))
          // There was no READY, so there is no session to resume.
          msg <- c2.nextJson
        yield msg
      }
    }
    TestControl.executeEmbed(program).map(m => assertEquals(m.hcursor.get[Int]("op"), Right(Opcode.Identify)))
  }

  test("commands wait for READY") {
    val program = FakeWs().flatMap { ws =>
      Gateway.connect(ws, "wss://gateway.example", config).use { gw =>
        for
          c <- ws.accept
          _ <- gw.updateVoiceState(Snowflake(2), Some(Snowflake(4)), selfDeaf = true)
          _ <- c.push(hello())
          first <- c.nextJson
          _ <- c.push(dispatch("READY", 1, readyData))
          second <- c.nextJson
        yield (first, second)
      }
    }
    TestControl.executeEmbed(program).map { (first, second) =>
      assertEquals(first.hcursor.get[Int]("op"), Right(Opcode.Identify))
      assertEquals(second.hcursor.get[Int]("op"), Right(Opcode.VoiceStateUpdate))
      assertEquals(
        second.hcursor.downField("d").as[Json],
        Right(Json.obj("guild_id" := "2", "channel_id" := "4", "self_mute" := false, "self_deaf" := true))
      )
    }
  }

  test("a fatal close code fails the event stream") {
    val program = FakeWs().flatMap { ws =>
      Gateway.connect(ws, "wss://gateway.example", config).use { gw =>
        gw.subscribe.use { events =>
          for
            c <- ws.accept
            _ <- c.push(hello())
            _ <- c.nextOp(Opcode.Identify)
            _ <- c.serverClose(4004)
            result <- events.compile.drain.attempt
          yield result
        }
      }
    }
    TestControl.executeEmbed(program).map {
      case Left(GatewayClosed(4004, _)) => ()
      case other                        => fail(s"Expected GatewayClosed(4004), got $other")
    }
  }
