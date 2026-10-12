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

package discord.gateway.ws

import munit.FunSuite
import scodec.bits.*

class FrameSuite extends FunSuite:

  test("masked frames round-trip at every length encoding") {
    val mask = hex"11223344"
    for len <- List(0, 1, 125, 126, 65535, 65536) do
      val frame = Frame(fin = true, Frame.Binary, ByteVector.fill(len.toLong)(7))
      val encoded = Frame.encode(frame, Some(mask))
      assertEquals(Frame.decode(encoded), Right(Some(frame -> ByteVector.empty)), s"length $len")
  }

  test("the RFC 6455 example: unmasked and masked 'Hello' text frames") {
    assertEquals(
      Frame.encode(Frame(true, Frame.Text, ByteVector("Hello".getBytes)), None),
      hex"810548656c6c6f"
    )
    assertEquals(
      Frame.encode(Frame(true, Frame.Text, ByteVector("Hello".getBytes)), Some(hex"37fa213d")),
      hex"818537fa213d7f9f4d5158"
    )
  }

  test("decoding waits for a complete frame and leaves the rest of the buffer") {
    val a = Frame.encode(Frame(true, Frame.Text, ByteVector("one".getBytes)), None)
    val b = Frame.encode(Frame(false, Frame.Text, ByteVector("two".getBytes)), None)
    val both = a ++ b
    assertEquals(Frame.decode(both.take(3)), Right(None))
    assertEquals(Frame.decode(both).map(_.map(_._2)), Right(Some(b)))
    assertEquals(Frame.decode(b).map(_.map(_._1.fin)), Right(Some(false)))
  }

  test("reserved bits are rejected") {
    assert(Frame.decode(hex"c100").isLeft)
  }

  test("SHA-1 matches known digests and the RFC 6455 accept example") {
    assertEquals(Sha1(ByteVector("abc".getBytes)), hex"a9993e364706816aba3e25717850c26c9cd0d89d")
    assertEquals(Sha1(ByteVector.empty), hex"da39a3ee5e6b4b0d3255bfef95601890afd80709")
    val accept = Sha1(ByteVector("dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11".getBytes))
    assertEquals(accept.toBase64, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
  }
