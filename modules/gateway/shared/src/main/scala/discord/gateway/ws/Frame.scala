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

import scodec.bits.ByteVector

/** A single RFC 6455 frame. */
final case class Frame(fin: Boolean, opcode: Int, payload: ByteVector)

object Frame:
  val Continuation = 0x0
  val Text = 0x1
  val Binary = 0x2
  val Close = 0x8
  val Ping = 0x9
  val Pong = 0xa

  def isControl(opcode: Int): Boolean = (opcode & 0x8) != 0

  /** Encodes a frame. Clients must mask every frame they send (`mask` is the 4-byte masking key). */
  def encode(frame: Frame, mask: Option[ByteVector]): ByteVector =
    val len = frame.payload.size
    val b0 = ((if frame.fin then 0x80 else 0) | (frame.opcode & 0x0f)).toByte
    val maskBit = if mask.isDefined then 0x80 else 0
    val lenBytes =
      if len < 126 then ByteVector((maskBit | len.toInt).toByte)
      else if len <= 0xffff then ByteVector((maskBit | 126).toByte) ++ ByteVector.fromInt(len.toInt, 2)
      else ByteVector((maskBit | 127).toByte) ++ ByteVector.fromLong(len, 8)
    mask match
      case Some(key) => ByteVector(b0) ++ lenBytes ++ key ++ applyMask(frame.payload, key)
      case None      => ByteVector(b0) ++ lenBytes ++ frame.payload

  def applyMask(payload: ByteVector, key: ByteVector): ByteVector =
    val in = payload.toArrayUnsafe
    val k = key.toArrayUnsafe
    val out = new Array[Byte](in.length)
    var i = 0
    while i < in.length do
      out(i) = (in(i) ^ k(i & 3)).toByte
      i += 1
    ByteVector.view(out)

  /** Decodes one frame from the front of `buf`. Returns `None` while more bytes are needed. */
  def decode(buf: ByteVector): Either[String, Option[(Frame, ByteVector)]] =
    if buf.size < 2 then Right(None)
    else
      val b0 = buf(0) & 0xff
      val b1 = buf(1) & 0xff
      if (b0 & 0x70) != 0 then Left("Reserved bits set, but no extension was negotiated")
      else
        val masked = (b1 & 0x80) != 0
        val len7 = b1 & 0x7f
        val extLen = if len7 == 126 then 2 else if len7 == 127 then 8 else 0
        val headerLen = 2 + extLen + (if masked then 4 else 0)
        if buf.size < headerLen then Right(None)
        else
          val len =
            if len7 == 126 then buf.slice(2, 4).toInt(signed = false).toLong
            else if len7 == 127 then buf.slice(2, 10).toLong()
            else len7.toLong
          if len < 0 then Left("Frame too large")
          else if buf.size < headerLen + len then Right(None)
          else
            val raw = buf.slice(headerLen, headerLen + len)
            val payload = if masked then applyMask(raw, buf.slice(2 + extLen, headerLen)) else raw
            Right(Some(Frame((b0 & 0x80) != 0, b0 & 0x0f, payload) -> buf.drop(headerLen + len)))
