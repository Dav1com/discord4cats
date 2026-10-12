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

/** AES-256-GCM with a 128-bit tag, backed by each platform's crypto: `javax.crypto` on the JVM, Node's
  * `crypto` module on Scala.js, and OpenSSL's libcrypto on Scala Native.
  */
trait AesGcm:
  /** Returns ciphertext followed by the 16-byte tag. */
  def encrypt(nonce: ByteVector, aad: ByteVector, plaintext: ByteVector): ByteVector

  /** Expects ciphertext followed by the tag. `None` if authentication fails. */
  def decrypt(nonce: ByteVector, aad: ByteVector, ciphertextAndTag: ByteVector): Option[ByteVector]

object AesGcm:
  def apply(key: ByteVector): AesGcm =
    require(key.size == 32, s"AES-256 needs a 32-byte key, got ${key.size}")
    AesGcmPlatform(key)
