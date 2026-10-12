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

/** One encoded Opus packet holding 20ms of 48kHz stereo audio, the format Discord expects. */
opaque type OpusFrame = ByteVector

object OpusFrame:
  def apply(bytes: ByteVector): OpusFrame = bytes
  extension (f: OpusFrame) def bytes: ByteVector = f

  /** Discord's recommended silence frame, sent when audio stops to avoid interpolation glitches. */
  val Silence: OpusFrame = ByteVector(0xf8, 0xff, 0xfe)

  /** Samples per channel in a 20ms frame at 48kHz: the RTP timestamp increment. */
  val SamplesPerFrame: Int = 960
