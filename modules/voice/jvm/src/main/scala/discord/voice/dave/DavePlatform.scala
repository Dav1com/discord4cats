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

import java.lang.foreign.*
import java.lang.foreign.ValueLayout.*
import java.lang.invoke.MethodHandle
import java.nio.file.Path

/** libdave through the Foreign Function & Memory API (JDK 22+). Run with
  * `--enable-native-access=ALL-UNNAMED`.
  *
  * The library is looked up as `libdave` on the system library path, or at `DAVE_LIBRARY_PATH` when set.
  */
private[dave] object DavePlatform:
  def create(): DaveSession = new LibDaveSession
  def maxProtocolVersion: Int = LibDave.maxProtocolVersion

private object LibDave:
  private val linker = Linker.nativeLinker()

  private val lookup: SymbolLookup = sys.env.get("DAVE_LIBRARY_PATH") match
    case Some(path) => SymbolLookup.libraryLookup(Path.of(path), Arena.global())
    case None       => SymbolLookup.libraryLookup(System.mapLibraryName("dave"), Arena.global())

  private def fn(name: String, result: MemoryLayout | Null, args: MemoryLayout*): MethodHandle =
    val symbol = lookup.find(name).orElseThrow(() => new UnsatisfiedLinkError(s"libdave has no symbol $name"))
    val descriptor =
      if result == null then FunctionDescriptor.ofVoid(args*) else FunctionDescriptor.of(result, args*)
    linker.downcallHandle(symbol, descriptor)

  private val P = ADDRESS
  private val SizeT = JAVA_LONG

  val MaxSupportedProtocolVersion = fn("daveMaxSupportedProtocolVersion", JAVA_SHORT)
  val Free = fn("daveFree", null, P)
  val SessionCreate = fn("daveSessionCreate", P, P, P, P, P)
  val SessionDestroy = fn("daveSessionDestroy", null, P)
  val SessionInit = fn("daveSessionInit", null, P, JAVA_SHORT, JAVA_LONG, P)
  val SessionReset = fn("daveSessionReset", null, P)
  val SessionGetProtocolVersion = fn("daveSessionGetProtocolVersion", JAVA_SHORT, P)
  val SessionSetExternalSender = fn("daveSessionSetExternalSender", null, P, P, SizeT)
  val SessionProcessProposals = fn("daveSessionProcessProposals", null, P, P, SizeT, P, SizeT, P, P)
  val SessionProcessCommit = fn("daveSessionProcessCommit", P, P, P, SizeT)
  val SessionProcessWelcome = fn("daveSessionProcessWelcome", P, P, P, SizeT, P, SizeT)
  val SessionGetMarshalledKeyPackage = fn("daveSessionGetMarshalledKeyPackage", null, P, P, P)
  val SessionGetKeyRatchet = fn("daveSessionGetKeyRatchet", P, P, P)
  val KeyRatchetDestroy = fn("daveKeyRatchetDestroy", null, P)
  val CommitResultIsFailed = fn("daveCommitResultIsFailed", JAVA_BOOLEAN, P)
  val CommitResultIsIgnored = fn("daveCommitResultIsIgnored", JAVA_BOOLEAN, P)
  val CommitResultDestroy = fn("daveCommitResultDestroy", null, P)
  val WelcomeResultDestroy = fn("daveWelcomeResultDestroy", null, P)
  val EncryptorCreate = fn("daveEncryptorCreate", P)
  val EncryptorDestroy = fn("daveEncryptorDestroy", null, P)
  val EncryptorSetKeyRatchet = fn("daveEncryptorSetKeyRatchet", null, P, P)
  val EncryptorSetPassthroughMode = fn("daveEncryptorSetPassthroughMode", null, P, JAVA_BOOLEAN)
  val EncryptorAssignSsrcToCodec = fn("daveEncryptorAssignSsrcToCodec", null, P, JAVA_INT, JAVA_INT)
  val EncryptorGetMaxCiphertextByteSize =
    fn("daveEncryptorGetMaxCiphertextByteSize", SizeT, P, JAVA_INT, SizeT)
  val EncryptorEncrypt = fn("daveEncryptorEncrypt", JAVA_INT, P, JAVA_INT, JAVA_INT, P, SizeT, P, SizeT, P)

  lazy val maxProtocolVersion: Int = call[Short](MaxSupportedProtocolVersion) & 0xffff

  def call[A](h: MethodHandle, args: Any*): A =
    h.invokeWithArguments(args.map(_.asInstanceOf[AnyRef])*).asInstanceOf[A]
  def run(h: MethodHandle, args: Any*): Unit =
    h.invokeWithArguments(args.map(_.asInstanceOf[AnyRef])*): Unit

  def bytes(arena: Arena, bv: ByteVector): MemorySegment =
    if bv.isEmpty then arena.allocate(1) else arena.allocateFrom(JAVA_BYTE, bv.toArray*)

  def strings(arena: Arena, values: Iterable[String]): MemorySegment =
    val arr = arena.allocate(P, values.size.max(1).toLong)
    values.zipWithIndex.foreach((s, i) => arr.setAtIndex(P, i.toLong, arena.allocateFrom(s)))
    arr

  /** Reads a buffer libdave allocated through `(uint8_t** out, size_t* len)`, then frees it. */
  def takeOutput(ptr: MemorySegment, len: MemorySegment): Option[ByteVector] =
    val p = ptr.get(P, 0)
    val n = len.get(SizeT, 0)
    if p == MemorySegment.NULL then None
    else
      try Option.when(n > 0)(ByteVector.view(p.reinterpret(n).toArray(JAVA_BYTE)))
      finally run(Free, p)

private final class LibDaveSession extends DaveSession:
  import LibDave.*

  private val AudioMediaType = 0
  private val OpusCodec = 1

  private val session: MemorySegment =
    call(SessionCreate, MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL)
  private val encryptor: MemorySegment = call(EncryptorCreate)
  private var selfUserId: String = ""

  def maxProtocolVersion: Int = LibDave.maxProtocolVersion

  def init(version: Int, groupId: Long, selfUserId: String): Unit =
    this.selfUserId = selfUserId
    val arena = Arena.ofConfined()
    try run(SessionInit, session, version.toShort, groupId, arena.allocateFrom(selfUserId))
    finally arena.close()

  def reset(): Unit = run(SessionReset, session)

  def protocolVersion: Int = call[Short](SessionGetProtocolVersion, session) & 0xffff

  def setExternalSender(data: ByteVector): Unit =
    val arena = Arena.ofConfined()
    try run(SessionSetExternalSender, session, bytes(arena, data), data.size)
    finally arena.close()

  def keyPackage(): ByteVector =
    val arena = Arena.ofConfined()
    try
      val out = arena.allocate(ADDRESS)
      val len = arena.allocate(JAVA_LONG)
      run(SessionGetMarshalledKeyPackage, session, out, len)
      takeOutput(out, len).getOrElse(throw new IllegalStateException("libdave returned no key package"))
    finally arena.close()

  def processProposals(payload: ByteVector, recognizedUserIds: Set[String]): Option[ByteVector] =
    val arena = Arena.ofConfined()
    try
      val out = arena.allocate(ADDRESS)
      val len = arena.allocate(JAVA_LONG)
      run(
        SessionProcessProposals,
        session,
        bytes(arena, payload),
        payload.size,
        strings(arena, recognizedUserIds),
        recognizedUserIds.size.toLong,
        out,
        len
      )
      takeOutput(out, len)
    finally arena.close()

  def processCommit(commit: ByteVector): CommitOutcome =
    val arena = Arena.ofConfined()
    try
      val result: MemorySegment = call(SessionProcessCommit, session, bytes(arena, commit), commit.size)
      if result == MemorySegment.NULL then CommitOutcome.Failed
      else
        try
          if call[Boolean](CommitResultIsFailed, result) then CommitOutcome.Failed
          else if call[Boolean](CommitResultIsIgnored, result) then CommitOutcome.Ignored
          else CommitOutcome.Joined
        finally run(CommitResultDestroy, result)
    finally arena.close()

  def processWelcome(welcome: ByteVector, recognizedUserIds: Set[String]): Boolean =
    val arena = Arena.ofConfined()
    try
      val result: MemorySegment = call(
        SessionProcessWelcome,
        session,
        bytes(arena, welcome),
        welcome.size,
        strings(arena, recognizedUserIds),
        recognizedUserIds.size.toLong
      )
      if result == MemorySegment.NULL then false
      else
        run(WelcomeResultDestroy, result)
        true
    finally arena.close()

  def useSelfKeyRatchet(version: Int): Unit =
    if version == 0 then run(EncryptorSetPassthroughMode, encryptor, true)
    else
      val arena = Arena.ofConfined()
      try
        val ratchet: MemorySegment = call(SessionGetKeyRatchet, session, arena.allocateFrom(selfUserId))
        if ratchet != MemorySegment.NULL then
          // The encryptor copies the ratchet, so ours can go.
          try run(EncryptorSetKeyRatchet, encryptor, ratchet)
          finally run(KeyRatchetDestroy, ratchet)
          run(EncryptorSetPassthroughMode, encryptor, false)
      finally arena.close()

  def assignOpusSsrc(ssrc: Int): Unit = run(EncryptorAssignSsrcToCodec, encryptor, ssrc, OpusCodec)

  def encryptOpus(ssrc: Int, frame: ByteVector): Option[ByteVector] =
    val capacity: Long = call(EncryptorGetMaxCiphertextByteSize, encryptor, AudioMediaType, frame.size)
    val arena = Arena.ofConfined()
    try
      val out = arena.allocate(capacity.max(1))
      val written = arena.allocate(JAVA_LONG)
      val rc: Int =
        call(
          EncryptorEncrypt,
          encryptor,
          AudioMediaType,
          ssrc,
          bytes(arena, frame),
          frame.size,
          out,
          capacity,
          written
        )
      Option.when(rc == 0)(ByteVector.view(out.asSlice(0, written.get(JAVA_LONG, 0)).toArray(JAVA_BYTE)))
    finally arena.close()

  def close(): Unit =
    run(EncryptorDestroy, encryptor)
    run(SessionDestroy, session)
