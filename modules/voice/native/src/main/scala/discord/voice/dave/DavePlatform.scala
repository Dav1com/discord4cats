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

import scodec.bits.ByteVector

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** libdave through Scala Native's C interop. Links against `libdave` (see `scripts/install-libdave.sh`). */
private[dave] object DavePlatform:
  def create(): DaveSession = new NativeDaveSession
  def maxProtocolVersion: Int = libdave.daveMaxSupportedProtocolVersion().toInt

@link("dave")
@extern
private object libdave:
  type Handle = Ptr[Byte]

  def daveMaxSupportedProtocolVersion(): CUnsignedShort = extern
  def daveFree(ptr: Ptr[Byte]): Unit = extern
  def daveSessionCreate(
      context: Ptr[Byte],
      authSessionId: CString,
      callback: Ptr[Byte],
      userData: Ptr[Byte]
  ): Handle = extern
  def daveSessionDestroy(session: Handle): Unit = extern
  def daveSessionInit(
      session: Handle,
      version: CUnsignedShort,
      groupId: CUnsignedLongLong,
      selfUserId: CString
  ): Unit = extern
  def daveSessionReset(session: Handle): Unit = extern
  def daveSessionGetProtocolVersion(session: Handle): CUnsignedShort = extern
  def daveSessionSetExternalSender(session: Handle, data: Ptr[Byte], length: CSize): Unit = extern
  def daveSessionProcessProposals(
      session: Handle,
      proposals: Ptr[Byte],
      length: CSize,
      recognizedUserIds: Ptr[CString],
      recognizedUserIdsLength: CSize,
      commitWelcome: Ptr[Ptr[Byte]],
      commitWelcomeLength: Ptr[CSize]
  ): Unit = extern
  def daveSessionProcessCommit(session: Handle, commit: Ptr[Byte], length: CSize): Handle = extern
  def daveSessionProcessWelcome(
      session: Handle,
      welcome: Ptr[Byte],
      length: CSize,
      recognizedUserIds: Ptr[CString],
      recognizedUserIdsLength: CSize
  ): Handle = extern
  def daveSessionGetMarshalledKeyPackage(
      session: Handle,
      keyPackage: Ptr[Ptr[Byte]],
      length: Ptr[CSize]
  ): Unit =
    extern
  def daveSessionGetKeyRatchet(session: Handle, userId: CString): Handle = extern
  def daveKeyRatchetDestroy(ratchet: Handle): Unit = extern
  def daveCommitResultIsFailed(result: Handle): CBool = extern
  def daveCommitResultIsIgnored(result: Handle): CBool = extern
  def daveCommitResultDestroy(result: Handle): Unit = extern
  def daveWelcomeResultDestroy(result: Handle): Unit = extern
  def daveEncryptorCreate(): Handle = extern
  def daveEncryptorDestroy(encryptor: Handle): Unit = extern
  def daveEncryptorSetKeyRatchet(encryptor: Handle, ratchet: Handle): Unit = extern
  def daveEncryptorSetPassthroughMode(encryptor: Handle, passthrough: CBool): Unit = extern
  def daveEncryptorAssignSsrcToCodec(encryptor: Handle, ssrc: CUnsignedInt, codec: CInt): Unit = extern
  def daveEncryptorGetMaxCiphertextByteSize(encryptor: Handle, mediaType: CInt, frameSize: CSize): CSize =
    extern
  def daveEncryptorEncrypt(
      encryptor: Handle,
      mediaType: CInt,
      ssrc: CUnsignedInt,
      frame: Ptr[Byte],
      frameLength: CSize,
      encryptedFrame: Ptr[Byte],
      capacity: CSize,
      bytesWritten: Ptr[CSize]
  ): CInt = extern

private final class NativeDaveSession extends DaveSession:
  import libdave.*

  private val AudioMediaType = 0
  private val OpusCodec = 1

  private val session = daveSessionCreate(null, null, null, null)
  private val encryptor = daveEncryptorCreate()
  private var selfUserId = ""

  private def copyIn(bv: ByteVector)(using Zone): Ptr[Byte] =
    val p = alloc[Byte](bv.size.max(1).toInt)
    var i = 0L
    while i < bv.size do
      p(i) = bv(i)
      i += 1
    p

  private def copyOut(p: Ptr[Byte], len: Long): ByteVector =
    val arr = new Array[Byte](len.toInt)
    var i = 0
    while i < arr.length do
      arr(i) = p(i.toLong)
      i += 1
    ByteVector.view(arr)

  private def cStrings(values: Iterable[String])(using Zone): Ptr[CString] =
    val arr = alloc[CString](values.size.max(1))
    values.zipWithIndex.foreach((s, i) => arr(i) = toCString(s))
    arr

  private def takeOutput(out: Ptr[Ptr[Byte]], len: Ptr[CSize]): Option[ByteVector] =
    val p = !out
    if p == null then None
    else
      try Option.when((!len).toLong > 0)(copyOut(p, (!len).toLong))
      finally daveFree(p)

  def maxProtocolVersion: Int = daveMaxSupportedProtocolVersion().toInt

  def init(version: Int, groupId: Long, selfUserId: String): Unit =
    this.selfUserId = selfUserId
    Zone(daveSessionInit(session, version.toUShort, groupId.toULong, toCString(selfUserId)))

  def reset(): Unit = daveSessionReset(session)

  def protocolVersion: Int = daveSessionGetProtocolVersion(session).toInt

  def setExternalSender(data: ByteVector): Unit =
    Zone(daveSessionSetExternalSender(session, copyIn(data), data.size.toCSize))

  def keyPackage(): ByteVector = Zone {
    val out = alloc[Ptr[Byte]]()
    val len = alloc[CSize]()
    daveSessionGetMarshalledKeyPackage(session, out, len)
    takeOutput(out, len).getOrElse(throw new IllegalStateException("libdave returned no key package"))
  }

  def processProposals(payload: ByteVector, recognizedUserIds: Set[String]): Option[ByteVector] = Zone {
    val out = alloc[Ptr[Byte]]()
    val len = alloc[CSize]()
    daveSessionProcessProposals(
      session,
      copyIn(payload),
      payload.size.toCSize,
      cStrings(recognizedUserIds),
      recognizedUserIds.size.toCSize,
      out,
      len
    )
    takeOutput(out, len)
  }

  def processCommit(commit: ByteVector): CommitOutcome = Zone {
    val result = daveSessionProcessCommit(session, copyIn(commit), commit.size.toCSize)
    if result == null then CommitOutcome.Failed
    else
      try
        if daveCommitResultIsFailed(result) then CommitOutcome.Failed
        else if daveCommitResultIsIgnored(result) then CommitOutcome.Ignored
        else CommitOutcome.Joined
      finally daveCommitResultDestroy(result)
  }

  def processWelcome(welcome: ByteVector, recognizedUserIds: Set[String]): Boolean = Zone {
    val result = daveSessionProcessWelcome(
      session,
      copyIn(welcome),
      welcome.size.toCSize,
      cStrings(recognizedUserIds),
      recognizedUserIds.size.toCSize
    )
    if result == null then false
    else
      daveWelcomeResultDestroy(result)
      true
  }

  def useSelfKeyRatchet(version: Int): Unit =
    if version == 0 then daveEncryptorSetPassthroughMode(encryptor, true)
    else
      val ratchet = Zone(daveSessionGetKeyRatchet(session, toCString(selfUserId)))
      if ratchet != null then
        try daveEncryptorSetKeyRatchet(encryptor, ratchet)
        finally daveKeyRatchetDestroy(ratchet)
        daveEncryptorSetPassthroughMode(encryptor, false)

  def assignOpusSsrc(ssrc: Int): Unit = daveEncryptorAssignSsrcToCodec(encryptor, ssrc.toUInt, OpusCodec)

  def encryptOpus(ssrc: Int, frame: ByteVector): Option[ByteVector] = Zone {
    val capacity = daveEncryptorGetMaxCiphertextByteSize(encryptor, AudioMediaType, frame.size.toCSize)
    val out = alloc[Byte](capacity.toLong.max(1).toInt)
    val written = alloc[CSize]()
    val rc = daveEncryptorEncrypt(
      encryptor,
      AudioMediaType,
      ssrc.toUInt,
      copyIn(frame),
      frame.size.toCSize,
      out,
      capacity,
      written
    )
    Option.when(rc == 0)(copyOut(out, (!written).toLong))
  }

  def close(): Unit =
    daveEncryptorDestroy(encryptor)
    daveSessionDestroy(session)
