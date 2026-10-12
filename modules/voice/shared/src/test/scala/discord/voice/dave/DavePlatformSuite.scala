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

package discord.voice.dave

import munit.FunSuite
import scodec.bits.*

/** Smoke test of the real DAVE implementation on this platform (libdave on JVM/Native, davey on Node). */
class DavePlatformSuite extends FunSuite:

  test("the platform DAVE implementation loads and supports protocol v1") {
    assert(DaveSession.maxProtocolVersion >= 1, DaveSession.maxProtocolVersion)
  }

  test("creates an MLS key package once initialized") {
    val s = DaveSession.create()
    try
      s.init(1, 1234567890123L, "123456789012345678")
      assertEquals(s.protocolVersion, 1)
      val kp = s.keyPackage()
      assert(kp.size > 100, s"Key package suspiciously small: ${kp.size} bytes")
      // A key package is fresh each time.
      assertNotEquals(s.keyPackage(), kp)
    finally s.close()
  }

  test("frames are dropped until there is a key, and pass through when DAVE is off") {
    val s = DaveSession.create()
    try
      s.assignOpusSsrc(1)
      val frame = hex"fc0102030405060708090a"
      assertEquals(s.encryptOpus(1, frame), None)
      s.useSelfKeyRatchet(0)
      assertEquals(s.encryptOpus(1, frame), Some(frame))
    finally s.close()
  }
