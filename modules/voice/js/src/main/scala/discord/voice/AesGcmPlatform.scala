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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

private[voice] object AesGcmPlatform:
  def apply(key: ByteVector): AesGcm = new AesGcm:
    private val k = key.toUint8Array

    def encrypt(nonce: ByteVector, aad: ByteVector, plaintext: ByteVector): ByteVector =
      val c = NodeCrypto.createCipheriv("aes-256-gcm", k, nonce.toUint8Array)
      c.setAAD(aad.toUint8Array)
      val body = ByteVector.view(c.update(plaintext.toUint8Array)) ++ ByteVector.view(c.`final`())
      body ++ ByteVector.view(c.getAuthTag())

    def decrypt(nonce: ByteVector, aad: ByteVector, ciphertextAndTag: ByteVector): Option[ByteVector] =
      if ciphertextAndTag.size < 16 then None
      else
        val d = NodeCrypto.createDecipheriv("aes-256-gcm", k, nonce.toUint8Array)
        d.setAAD(aad.toUint8Array)
        d.setAuthTag(ciphertextAndTag.takeRight(16).toUint8Array)
        try
          val out = ByteVector.view(d.update(ciphertextAndTag.dropRight(16).toUint8Array))
          Some(out ++ ByteVector.view(d.`final`()))
        catch case _: js.JavaScriptException => None

@js.native
@JSImport("crypto", JSImport.Namespace)
private object NodeCrypto extends js.Object:
  def createCipheriv(algorithm: String, key: Uint8Array, iv: Uint8Array): NodeCipher = js.native
  def createDecipheriv(algorithm: String, key: Uint8Array, iv: Uint8Array): NodeDecipher = js.native

@js.native
private trait NodeCipher extends js.Object:
  def setAAD(aad: Uint8Array): Unit = js.native
  def update(data: Uint8Array): Uint8Array = js.native
  def `final`(): Uint8Array = js.native
  def getAuthTag(): Uint8Array = js.native

@js.native
private trait NodeDecipher extends js.Object:
  def setAAD(aad: Uint8Array): Unit = js.native
  def setAuthTag(tag: Uint8Array): Unit = js.native
  def update(data: Uint8Array): Uint8Array = js.native
  def `final`(): Uint8Array = js.native
