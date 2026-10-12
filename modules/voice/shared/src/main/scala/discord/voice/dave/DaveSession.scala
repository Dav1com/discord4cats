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

/** Result of processing an MLS commit. */
enum CommitOutcome:
  /** The commit was applied and we're in the group. */
  case Joined

  /** The commit wasn't meant for our group state; nothing to do. */
  case Ignored
  case Failed

/** Platform binding to a DAVE implementation: libdave on the JVM (Panama FFM) and Scala Native (`@extern`),
  * and `@snazzah/davey` on Node.js.
  *
  * This is a thin, impure and not thread-safe wrapper. [[DaveManager]] serializes every call.
  */
trait DaveSession:
  /** Highest DAVE protocol version the implementation supports (sent in the voice Identify). */
  def maxProtocolVersion: Int

  /** Starts a new MLS group state for `groupId` (the voice channel ID). */
  def init(version: Int, groupId: Long, selfUserId: String): Unit
  def reset(): Unit
  def protocolVersion: Int
  def setExternalSender(data: ByteVector): Unit

  /** A fresh key package to send in opcode 26. */
  def keyPackage(): ByteVector

  /** Processes an opcode 27 payload (operation type byte + proposals). Returns the opcode 28 payload (commit,
    * then welcome if any), if there is something to commit.
    */
  def processProposals(payload: ByteVector, recognizedUserIds: Set[String]): Option[ByteVector]
  def processCommit(commit: ByteVector): CommitOutcome

  /** `true` if we joined the group. */
  def processWelcome(welcome: ByteVector, recognizedUserIds: Set[String]): Boolean

  /** Switches our own media encryption to the current group's key (or to passthrough when `version` is 0). */
  def useSelfKeyRatchet(version: Int): Unit

  def assignOpusSsrc(ssrc: Int): Unit

  /** End-to-end encrypts an Opus frame. `None` means the frame must be dropped (no key yet). */
  def encryptOpus(ssrc: Int, frame: ByteVector): Option[ByteVector]

  def close(): Unit

object DaveSession:
  def create(): DaveSession = DavePlatform.create()

  /** The highest protocol version the platform's DAVE implementation supports. */
  def maxProtocolVersion: Int = DavePlatform.maxProtocolVersion
