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

import scala.scalanative.unsafe.*

private[voice] object AesGcmPlatform:
  import libcrypto.*

  private val EVP_CTRL_GCM_SET_IVLEN = 0x9
  private val EVP_CTRL_GCM_GET_TAG = 0x10
  private val EVP_CTRL_GCM_SET_TAG = 0x11

  def apply(key: ByteVector): AesGcm = new AesGcm:
    def encrypt(nonce: ByteVector, aad: ByteVector, plaintext: ByteVector): ByteVector = Zone {
      val ctx = EVP_CIPHER_CTX_new()
      try
        val out = alloc[Byte]((plaintext.size + 16).toInt)
        val outl = alloc[CInt]()
        check(EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), null, null, null))
        check(EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, nonce.size.toInt, null))
        check(EVP_EncryptInit_ex(ctx, null, null, toPtr(key), toPtr(nonce)))
        check(EVP_EncryptUpdate(ctx, null, outl, toPtr(aad), aad.size.toInt))
        check(EVP_EncryptUpdate(ctx, out, outl, toPtr(plaintext), plaintext.size.toInt))
        val written = !outl
        check(EVP_EncryptFinal_ex(ctx, out + written, outl))
        val total = written + !outl
        val tag = alloc[Byte](16)
        check(EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16, tag))
        fromPtr(out, total) ++ fromPtr(tag, 16)
      finally EVP_CIPHER_CTX_free(ctx)
    }

    def decrypt(nonce: ByteVector, aad: ByteVector, ciphertextAndTag: ByteVector): Option[ByteVector] =
      if ciphertextAndTag.size < 16 then None
      else
        Zone {
          val ctx = EVP_CIPHER_CTX_new()
          try
            val ct = ciphertextAndTag.dropRight(16)
            val out = alloc[Byte]((ct.size + 16).toInt)
            val outl = alloc[CInt]()
            check(EVP_DecryptInit_ex(ctx, EVP_aes_256_gcm(), null, null, null))
            check(EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, nonce.size.toInt, null))
            check(EVP_DecryptInit_ex(ctx, null, null, toPtr(key), toPtr(nonce)))
            check(EVP_DecryptUpdate(ctx, null, outl, toPtr(aad), aad.size.toInt))
            check(EVP_DecryptUpdate(ctx, out, outl, toPtr(ct), ct.size.toInt))
            val written = !outl
            check(EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, 16, toPtr(ciphertextAndTag.takeRight(16))))
            if EVP_DecryptFinal_ex(ctx, out + written, outl) == 1 then Some(fromPtr(out, written + !outl))
            else None
          finally EVP_CIPHER_CTX_free(ctx)
        }

  private def check(rc: CInt): Unit =
    if rc != 1 then throw new IllegalStateException(s"OpenSSL AES-GCM call failed ($rc)")

  private[voice] def toPtr(bv: ByteVector)(using Zone): Ptr[Byte] =
    val p = alloc[Byte](bv.size.max(1).toInt)
    var i = 0L
    while i < bv.size do
      p(i) = bv(i)
      i += 1
    p

  private[voice] def fromPtr(p: Ptr[Byte], len: Long): ByteVector =
    val arr = new Array[Byte](len.toInt)
    var i = 0
    while i < arr.length do
      arr(i) = p(i.toLong)
      i += 1
    ByteVector.view(arr)

@link("crypto")
@extern
private object libcrypto:
  def EVP_CIPHER_CTX_new(): Ptr[Byte] = extern
  def EVP_CIPHER_CTX_free(ctx: Ptr[Byte]): Unit = extern
  def EVP_aes_256_gcm(): Ptr[Byte] = extern
  def EVP_CIPHER_CTX_ctrl(ctx: Ptr[Byte], tpe: CInt, arg: CInt, ptr: Ptr[Byte]): CInt = extern
  def EVP_EncryptInit_ex(
      ctx: Ptr[Byte],
      cipher: Ptr[Byte],
      engine: Ptr[Byte],
      key: Ptr[Byte],
      iv: Ptr[Byte]
  ): CInt =
    extern
  def EVP_EncryptUpdate(ctx: Ptr[Byte], out: Ptr[Byte], outl: Ptr[CInt], in: Ptr[Byte], inl: CInt): CInt =
    extern
  def EVP_EncryptFinal_ex(ctx: Ptr[Byte], out: Ptr[Byte], outl: Ptr[CInt]): CInt = extern
  def EVP_DecryptInit_ex(
      ctx: Ptr[Byte],
      cipher: Ptr[Byte],
      engine: Ptr[Byte],
      key: Ptr[Byte],
      iv: Ptr[Byte]
  ): CInt =
    extern
  def EVP_DecryptUpdate(ctx: Ptr[Byte], out: Ptr[Byte], outl: Ptr[CInt], in: Ptr[Byte], inl: CInt): CInt =
    extern
  def EVP_DecryptFinal_ex(ctx: Ptr[Byte], out: Ptr[Byte], outl: Ptr[CInt]): CInt = extern
