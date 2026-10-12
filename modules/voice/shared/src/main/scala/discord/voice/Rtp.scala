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

import scodec.bits.ByteVector

/** RTP state for one outgoing audio stream. Sequence numbers and timestamps wrap as unsigned values. */
final case class RtpState(ssrc: Int, sequence: Int, timestamp: Int):
  /** The 12-byte RTP header Discord expects: version 2, payload type 0x78 (Opus). */
  def header: ByteVector =
    ByteVector(0x80, 0x78) ++ ByteVector.fromInt(sequence & 0xffff, 2) ++ ByteVector.fromInt(timestamp) ++
      ByteVector.fromInt(ssrc)

  def next: RtpState =
    copy(sequence = (sequence + 1) & 0xffff, timestamp = timestamp + OpusFrame.SamplesPerFrame)

object RtpState:
  def initial(ssrc: Int): RtpState = RtpState(ssrc, 0, 0)

/** Builds encrypted voice packets for the `aead_aes256_gcm_rtpsize` mode.
  *
  * The RTP header is sent in the clear and authenticated as additional data. The 12-byte nonce is a 32-bit
  * big-endian counter padded with zeros, and the counter is appended to the packet.
  */
object VoicePacket:
  val Mode = "aead_aes256_gcm_rtpsize"

  def nonce(counter: Int): ByteVector = ByteVector.fromInt(counter) ++ ByteVector.low(8)

  def encrypt(cipher: AesGcm, rtp: RtpState, counter: Int, payload: ByteVector): ByteVector =
    val header = rtp.header
    header ++ cipher.encrypt(nonce(counter), header, payload) ++ ByteVector.fromInt(counter)

  /** Inverse of [[encrypt]], for tests. Returns the RTP header and the payload. */
  def decrypt(cipher: AesGcm, packet: ByteVector): Option[(ByteVector, ByteVector)] =
    if packet.size < 12 + 16 + 4 then None
    else
      val header = packet.take(12)
      val counter = packet.takeRight(4)
      cipher.decrypt(counter ++ ByteVector.low(8), header, packet.drop(12).dropRight(4)).map(header -> _)
