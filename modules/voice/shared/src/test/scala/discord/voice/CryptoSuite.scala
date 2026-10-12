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

import munit.FunSuite
import scodec.bits.*

class CryptoSuite extends FunSuite:
  private val zeroKey = ByteVector.low(32)
  private val zeroIv = ByteVector.low(12)

  test("AES-256-GCM matches the GCM spec test vectors (cases 13 and 14)") {
    val gcm = AesGcm(zeroKey)
    assertEquals(
      gcm.encrypt(zeroIv, ByteVector.empty, ByteVector.empty),
      hex"530f8afbc74536b9a963b4f1c4cb738b"
    )
    assertEquals(
      gcm.encrypt(zeroIv, ByteVector.empty, ByteVector.low(16)),
      hex"cea7403d4d606b6e074ec5d3baf39d18d0d1c8a799996bf0265b98b5d48ab919"
    )
  }

  test("AES-256-GCM round-trips with additional data and rejects tampering") {
    val gcm = AesGcm(ByteVector.fromValidHex("00" * 31 + "01"))
    val nonce = VoicePacket.nonce(7)
    val sealed_ = gcm.encrypt(nonce, hex"cafe", hex"0102030405")
    assertEquals(gcm.decrypt(nonce, hex"cafe", sealed_), Some(hex"0102030405"))
    assertEquals(gcm.decrypt(nonce, hex"beef", sealed_), None)
    assertEquals(gcm.decrypt(nonce, hex"cafe", sealed_.update(0, (sealed_(0) ^ 1).toByte)), None)
  }

  test("RTP headers carry version 2, the Opus payload type, sequence, timestamp and SSRC") {
    val rtp = RtpState(ssrc = 0x01020304, sequence = 0xffff, timestamp = -960)
    assertEquals(rtp.header, hex"8078ffff fffffc40 01020304")
    val next = rtp.next
    assertEquals((next.sequence, next.timestamp), (0, 0))
  }

  test("voice packets: clear RTP header, sealed payload, then the 4-byte nonce counter") {
    val gcm = AesGcm(zeroKey)
    val rtp = RtpState.initial(42)
    val packet = VoicePacket.encrypt(gcm, rtp, 5, hex"f8fffe")
    assertEquals(packet.take(12), rtp.header)
    assertEquals(packet.takeRight(4), hex"00000005")
    assertEquals(packet.size, 12L + 3 + 16 + 4)
    assertEquals(VoicePacket.decrypt(gcm, packet), Some(rtp.header -> hex"f8fffe"))
  }
