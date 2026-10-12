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

/** SHA-1, only used to verify `Sec-WebSocket-Accept` during the handshake (not for security). Pure Scala so
  * it works the same on every platform.
  */
private[ws] object Sha1:
  def apply(data: ByteVector): ByteVector =
    val msg = data.toArray
    val bitLen = msg.length.toLong * 8
    val padLen = ((56 - (msg.length + 1) % 64) + 64) % 64
    val padded = msg ++ Array(0x80.toByte) ++ new Array[Byte](padLen) ++ ByteVector.fromLong(bitLen).toArray
    var h0 = 0x67452301
    var h1 = 0xefcdab89
    var h2 = 0x98badcfe
    var h3 = 0x10325476
    var h4 = 0xc3d2e1f0
    val w = new Array[Int](80)
    var block = 0
    while block < padded.length do
      var i = 0
      while i < 16 do
        val j = block + i * 4
        w(i) = ((padded(j) & 0xff) << 24) | ((padded(j + 1) & 0xff) << 16) | ((padded(j + 2) & 0xff) << 8) |
          (padded(j + 3) & 0xff)
        i += 1
      while i < 80 do
        w(i) = Integer.rotateLeft(w(i - 3) ^ w(i - 8) ^ w(i - 14) ^ w(i - 16), 1)
        i += 1
      var a = h0; var b = h1; var c = h2; var d = h3; var e = h4
      i = 0
      while i < 80 do
        val (f, k) =
          if i < 20 then ((b & c) | (~b & d), 0x5a827999)
          else if i < 40 then (b ^ c ^ d, 0x6ed9eba1)
          else if i < 60 then ((b & c) | (b & d) | (c & d), 0x8f1bbcdc)
          else (b ^ c ^ d, 0xca62c1d6)
        val t = Integer.rotateLeft(a, 5) + f + e + k + w(i)
        e = d; d = c; c = Integer.rotateLeft(b, 30); b = a; a = t
        i += 1
      h0 += a; h1 += b; h2 += c; h3 += d; h4 += e
      block += 64
    ByteVector.fromInt(h0) ++ ByteVector.fromInt(h1) ++ ByteVector.fromInt(h2) ++ ByteVector.fromInt(h3) ++
      ByteVector.fromInt(h4)
