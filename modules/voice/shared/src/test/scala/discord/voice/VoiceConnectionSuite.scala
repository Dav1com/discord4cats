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

package discord.voice

import cats.effect.{IO, Resource}
import cats.effect.std.Queue
import discord.gateway.FakeWs
import discord.model.Snowflake
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import scodec.bits.*

class VoiceConnectionSuite extends CatsEffectSuite:

  private val key = ByteVector((0 until 32).map(_.toByte))
  private val ssrc = 7

  private def discoveryResponse =
    ByteVector.fromInt(2, 2) ++ ByteVector.fromInt(70, 2) ++ ByteVector.fromInt(ssrc) ++
      ByteVector("203.0.113.5".getBytes).padRight(64) ++ ByteVector.fromInt(50000, 2)

  test("identifies, discovers its IP, selects the protocol and streams encrypted, paced audio") {
    for
      ws <- FakeWs()
      udpOut <- Queue.unbounded[IO, ByteVector]
      udp = (_: String, _: Int) =>
        Resource.pure[IO, UdpSocket[IO]](new UdpSocket[IO]:
          def send(packet: ByteVector) = udpOut.offer(packet)
          def receive = IO.pure(discoveryResponse))
      server <- (for
        c <- ws.accept
        identify <- c.nextOp(0)
        _ <- c.push(Json.obj("op" := 8, "d" := Json.obj("heartbeat_interval" := 5000)))
        _ <- c.push(
          Json.obj(
            "op" := 2,
            "d" := Json.obj(
              "ssrc" := ssrc,
              "ip" := "198.51.100.1",
              "port" := 4000,
              "modes" := List("aead_aes256_gcm_rtpsize", "aead_xchacha20_poly1305_rtpsize")
            )
          )
        )
        select <- c.nextOp(1)
        _ <- c.push(
          Json.obj(
            "op" := 4,
            "d" := Json.obj(
              "mode" := "aead_aes256_gcm_rtpsize",
              "secret_key" := key.toArray.map(_ & 0xff).toList,
              "dave_protocol_version" := 0
            )
          )
        )
        speakingOn <- c.nextOp(5)
        speakingOff <- c.nextOp(5)
      yield (c.uri.renderString, identify, select, speakingOn, speakingOff)).start
      info = Voice.ServerInfo("sess", "vtok", "voice.example:443")
      start <- IO.monotonic
      version <- Voice
        .connect[IO](ws, info, Snowflake(1), Snowflake(2), Snowflake(3), NoOpLogger[IO], Some(udp))
        .use(vc => vc.play(Stream(OpusFrame(hex"fc01"), OpusFrame(hex"fc02"))) >> vc.daveProtocolVersion)
      elapsed <- IO.monotonic.map(_ - start)
      serverSaw <- server.joinWithNever
      (uri, identify, select, on, off) = serverSaw
      packets <- udpOut.tryTakeN(None)
    yield
      assertEquals(uri, "wss://voice.example:443?v=8")
      assertEquals(
        identify.hcursor.downField("d").as[Json],
        Right(
          Json.obj(
            "server_id" := "1",
            "user_id" := "3",
            "session_id" := "sess",
            "token" := "vtok",
            "max_dave_protocol_version" := discord.voice.dave.DaveSession.maxProtocolVersion
          )
        )
      )
      assertEquals(
        select.hcursor.downField("d").as[Json],
        Right(
          Json.obj(
            "protocol" := "udp",
            "data" := Json.obj(
              "address" := "203.0.113.5",
              "port" := 50000,
              "mode" := "aead_aes256_gcm_rtpsize"
            )
          )
        )
      )
      assertEquals(on.hcursor.downField("d").get[Int]("speaking"), Right(1))
      assertEquals(off.hcursor.downField("d").get[Int]("speaking"), Right(0))
      assertEquals(version, 0)

      // IP discovery request, then 2 frames and 5 silence frames.
      assertEquals(packets.head.size, 74L)
      val cipher = AesGcm(key)
      val audio = packets.tail.map(p => VoicePacket.decrypt(cipher, p).getOrElse(fail("Could not decrypt")))
      assertEquals(audio.map(_._2), List(hex"fc01", hex"fc02") ++ List.fill(5)(OpusFrame.Silence.bytes))
      assertEquals(audio.map(_._1.slice(2, 4).toInt(signed = false)), (0 until 7).toList)
      assertEquals(audio.map(_._1.slice(4, 8).toInt()), (0 until 7).map(_ * 960).toList)
      // 7 frames at one per 20ms, the first immediately.
      assert(elapsed.toMillis >= 120, elapsed)
  }
