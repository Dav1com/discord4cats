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

import javax.crypto.{AEADBadTagException, Cipher}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

private[voice] object AesGcmPlatform:
  def apply(key: ByteVector): AesGcm = new AesGcm:
    private val secret = new SecretKeySpec(key.toArray, "AES")

    private def cipher(mode: Int, nonce: ByteVector, aad: ByteVector): Cipher =
      val c = Cipher.getInstance("AES/GCM/NoPadding")
      c.init(mode, secret, new GCMParameterSpec(128, nonce.toArray))
      c.updateAAD(aad.toArray)
      c

    def encrypt(nonce: ByteVector, aad: ByteVector, plaintext: ByteVector): ByteVector =
      ByteVector.view(cipher(Cipher.ENCRYPT_MODE, nonce, aad).doFinal(plaintext.toArray))

    def decrypt(nonce: ByteVector, aad: ByteVector, ciphertextAndTag: ByteVector): Option[ByteVector] =
      try Some(ByteVector.view(cipher(Cipher.DECRYPT_MODE, nonce, aad).doFinal(ciphertextAndTag.toArray)))
      catch case _: AEADBadTagException => None
