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

import cats.effect.IO
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import scodec.bits.*

class OggOpusSuite extends CatsEffectSuite:

  /** An Ogg page. Each packet is laced into 255-byte segments; `open` leaves the last packet unterminated, so
    * it continues on the next page. The CRC is left at zero: the demuxer doesn't check it.
    */
  private def page(packets: List[ByteVector], continued: Boolean = false, open: Boolean = false): ByteVector =
    val lacing = packets.zipWithIndex.flatMap { (p, i) =>
      val full = List.fill((p.size / 255).toInt)(255)
      if open && i == packets.size - 1 then full else full :+ (p.size % 255).toInt
    }
    hex"4f676753 00" ++ ByteVector(if continued then 1 else 0) ++ ByteVector.low(8) ++ ByteVector.low(4) ++
      ByteVector.low(4) ++ ByteVector.low(4) ++ ByteVector(lacing.size) ++ ByteVector(lacing.map(_.toByte)) ++
      packets.foldLeft(ByteVector.empty)(_ ++ _)

  private val head = ByteVector("OpusHead".getBytes) ++ hex"0102380180bb000000000000"
  private val tags = ByteVector("OpusTags".getBytes) ++ ByteVector.low(8)
  private val big = ByteVector.fill(600)(9)

  test("extracts Opus packets, skipping headers and joining packets split across pages") {
    val bytes = page(List(head)) ++ page(List(tags)) ++
      page(List(hex"aa", big.take(255)), open = true) ++ page(List(big.drop(255), hex"bb"), continued = true)
    // Feed it in small chunks to exercise buffering.
    Stream
      .chunk(Chunk.byteVector(bytes))
      .chunkLimit(7)
      .unchunks
      .covary[IO]
      .through(OggOpus.frames)
      .map(_.bytes)
      .compile
      .toList
      .map(frames => assertEquals(frames, List(hex"aa", big, hex"bb")))
  }

  test("rejects streams that aren't Ogg Opus") {
    Stream
      .chunk(Chunk.byteVector(page(List(ByteVector("vorbis".getBytes)))))
      .covary[IO]
      .through(OggOpus.frames)
      .compile
      .drain
      .intercept[OggOpus.OggException]
  }
