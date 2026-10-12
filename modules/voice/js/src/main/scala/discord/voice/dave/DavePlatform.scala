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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

/** DAVE on Node.js through [[https://github.com/Snazzah/davey `@snazzah/davey`]], the implementation
  * discord.js uses. Discord doesn't publish libdave for JavaScript runtimes. Install it next to your app:
  * `npm install @snazzah/davey`.
  */
private[dave] object DavePlatform:
  def create(): DaveSession = new DaveyDaveSession
  def maxProtocolVersion: Int = Davey.DAVE_PROTOCOL_VERSION

@js.native
@JSImport("@snazzah/davey", JSImport.Namespace)
private object Davey extends js.Object:
  val DAVE_PROTOCOL_VERSION: Int = js.native

@js.native
@JSImport("@snazzah/davey", "DAVESession")
@annotation.nowarn("msg=unused")
private class DaveyJsSession(protocolVersion0: Int, userId0: String, channelId0: String) extends js.Object:
  def reinit(protocolVersion: Int, userId: String, channelId: String): Unit = js.native
  def reset(): Unit = js.native
  def protocolVersion: Int = js.native
  def ready: Boolean = js.native
  def setExternalSender(data: js.Any): Unit = js.native
  def getSerializedKeyPackage(): Uint8Array = js.native
  def processProposals(
      operationType: Int,
      proposals: js.Any,
      recognizedUserIds: js.Array[String]
  ): ProposalsResult =
    js.native
  def processWelcome(welcome: js.Any): Unit = js.native
  def processCommit(commit: js.Any): Unit = js.native
  def encryptOpus(packet: js.Any): Uint8Array = js.native

@js.native
private trait ProposalsResult extends js.Object:
  val commit: js.UndefOr[Uint8Array] = js.native
  val welcome: js.UndefOr[Uint8Array] = js.native

@js.native
@JSImport("buffer", "Buffer")
private object NodeBuffer extends js.Object:
  def from(data: Uint8Array): js.Any = js.native

private final class DaveyDaveSession extends DaveSession:
  private var session: DaveyJsSession | Null = null
  // Like libdave's encryptor: drop frames until a key ratchet (or passthrough) has been set.
  private var passthrough = false

  private def buf(bv: ByteVector): js.Any = NodeBuffer.from(bv.toUint8Array)
  private def bytes(u: Uint8Array): ByteVector = ByteVector.view(u)

  private def current: DaveyJsSession =
    val s = session
    if s == null then throw new IllegalStateException("DAVE session not initialized") else s

  def maxProtocolVersion: Int = Davey.DAVE_PROTOCOL_VERSION

  def init(version: Int, groupId: Long, selfUserId: String): Unit =
    val s = session
    if s == null then
      session = new DaveyJsSession(version, selfUserId, java.lang.Long.toUnsignedString(groupId))
    else s.reinit(version, selfUserId, java.lang.Long.toUnsignedString(groupId))

  def reset(): Unit =
    val s = session
    if s != null then s.reset()

  def protocolVersion: Int =
    val s = session
    if s == null then 0 else s.protocolVersion

  def setExternalSender(data: ByteVector): Unit = current.setExternalSender(buf(data))

  def keyPackage(): ByteVector = bytes(current.getSerializedKeyPackage())

  def processProposals(payload: ByteVector, recognizedUserIds: Set[String]): Option[ByteVector] =
    if payload.isEmpty then None
    else
      val result =
        current.processProposals(payload(0) & 0xff, buf(payload.drop(1)), js.Array(recognizedUserIds.toSeq*))
      result.commit.toOption.map(c => bytes(c) ++ result.welcome.toOption.fold(ByteVector.empty)(bytes))

  def processCommit(commit: ByteVector): CommitOutcome =
    try
      current.processCommit(buf(commit))
      CommitOutcome.Joined
    catch case _: js.JavaScriptException => CommitOutcome.Failed

  def processWelcome(welcome: ByteVector, recognizedUserIds: Set[String]): Boolean =
    try
      current.processWelcome(buf(welcome))
      true
    catch case _: js.JavaScriptException => false

  def useSelfKeyRatchet(version: Int): Unit = passthrough = version == 0

  def assignOpusSsrc(ssrc: Int): Unit = ()

  def encryptOpus(ssrc: Int, frame: ByteVector): Option[ByteVector] =
    if passthrough then Some(frame)
    else
      val s = session
      if s == null || !s.ready then None
      else
        try Some(bytes(s.encryptOpus(buf(frame))))
        catch case _: js.JavaScriptException => None

  def close(): Unit = reset()
