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

import cats.effect.{Async, Ref}
import cats.effect.std.Mutex
import cats.syntax.all.*
import scodec.bits.ByteVector

/** Messages the DAVE state machine needs to send on the voice gateway. */
enum DaveOutbound:
  /** Opcode 26, binary. */
  case KeyPackage(data: ByteVector)

  /** Opcode 28, binary. */
  case CommitWelcome(data: ByteVector)

  /** Opcode 23, JSON. */
  case TransitionReady(transitionId: Int)

  /** Opcode 31, JSON. */
  case InvalidCommitWelcome(transitionId: Int)

/** Drives a [[DaveSession]] from voice gateway messages. A port of libdave's reference `DaveSessionManager`
  * (samples/typescript), reduced to what a sender needs: only our own key ratchet is tracked.
  */
final class DaveManager[F[_]] private (
    session: DaveSession,
    selfUserId: String,
    groupId: Long,
    send: DaveOutbound => F[Unit],
    lock: Mutex[F],
    recognized: Ref[F, Set[String]],
    transitions: Ref[F, Map[Int, Int]],
    activeVersion: Ref[F, Int]
)(using F: Async[F]):
  import DaveManager.*

  private def locked[A](thunk: => A): F[A] = lock.lock.surround(F.delay(thunk))

  private def recognizedIds: F[Set[String]] = recognized.get.map(_ + selfUserId)

  /** The protocol version our media is currently sent with. 0 means not end-to-end encrypted. */
  def protocolVersion: F[Int] = activeVersion.get

  private def sessionVersion: F[Int] = locked(session.protocolVersion)

  def assignOpusSsrc(ssrc: Int): F[Unit] = locked(session.assignOpusSsrc(ssrc))

  def encryptOpus(ssrc: Int, frame: ByteVector): F[Option[ByteVector]] = locked(
    session.encryptOpus(ssrc, frame)
  )

  def onClientsConnect(userIds: List[String]): F[Unit] = recognized.update(_ ++ userIds)

  def onClientDisconnect(userId: String): F[Unit] = recognized.update(_ - userId)

  /** Opcode 4 (session description) carries the negotiated protocol version. */
  def onSessionDescription(version: Int): F[Unit] = handleInit(version)

  /** Opcode 21. */
  def onPrepareTransition(transitionId: Int, version: Int): F[Unit] =
    prepareRatchets(transitionId, version) >> readyForTransition(transitionId)

  /** Opcode 22. */
  def onExecuteTransition(transitionId: Int): F[Unit] = executeTransition(transitionId)

  /** Opcode 24. */
  def onPrepareEpoch(epoch: Long, version: Int): F[Unit] =
    (locked(session.init(version, groupId, selfUserId)) >> sendKeyPackage).whenA(epoch == NewGroupEpoch)

  /** Opcode 25. */
  def onExternalSender(data: ByteVector): F[Unit] = locked(session.setExternalSender(data))

  /** Opcode 27. */
  def onProposals(payload: ByteVector): F[Unit] =
    recognizedIds.flatMap(ids => locked(session.processProposals(payload, ids))).flatMap {
      _.traverse_(cw => send(DaveOutbound.CommitWelcome(cw)))
    }

  /** Opcode 29. */
  def onAnnounceCommit(transitionId: Int, commit: ByteVector): F[Unit] =
    locked(session.processCommit(commit)).flatMap {
      case CommitOutcome.Ignored => F.unit
      case CommitOutcome.Joined  =>
        sessionVersion.flatMap(prepareRatchets(transitionId, _)) >> readyForTransition(transitionId)
      case CommitOutcome.Failed =>
        send(DaveOutbound.InvalidCommitWelcome(transitionId)) >> sessionVersion.flatMap(handleInit)
    }

  /** Opcode 30. */
  def onWelcome(transitionId: Int, welcome: ByteVector): F[Unit] =
    recognizedIds.flatMap(ids => locked(session.processWelcome(welcome, ids))).flatMap {
      case true =>
        sessionVersion.flatMap(prepareRatchets(transitionId, _)) >> readyForTransition(transitionId)
      case false =>
        send(DaveOutbound.InvalidCommitWelcome(transitionId)) >> sendKeyPackage
    }

  private def sendKeyPackage: F[Unit] =
    locked(session.keyPackage()).flatMap(kp => send(DaveOutbound.KeyPackage(kp)))

  private def readyForTransition(transitionId: Int): F[Unit] =
    send(DaveOutbound.TransitionReady(transitionId)).unlessA(transitionId == InitTransitionId)

  private def handleInit(version: Int): F[Unit] =
    if version > 0 then onPrepareEpoch(NewGroupEpoch, version)
    else prepareRatchets(InitTransitionId, version) >> executeTransition(InitTransitionId)

  private def useSelfKeyRatchet(version: Int): F[Unit] =
    locked(session.useSelfKeyRatchet(version)) >> activeVersion.set(version)

  private def prepareRatchets(transitionId: Int, version: Int): F[Unit] =
    if transitionId == InitTransitionId then useSelfKeyRatchet(version)
    else transitions.update(_ + (transitionId -> version))

  private def executeTransition(transitionId: Int): F[Unit] =
    transitions.modify(t => (t - transitionId, t.get(transitionId))).flatMap {
      case None          => F.unit
      case Some(version) =>
        locked(session.reset()).whenA(version == DisabledVersion) >> useSelfKeyRatchet(version)
    }

object DaveManager:
  val InitTransitionId = 0
  val DisabledVersion = 0
  val NewGroupEpoch = 1L

  def apply[F[_]: Async](
      session: DaveSession,
      selfUserId: String,
      groupId: Long,
      send: DaveOutbound => F[Unit]
  ): F[DaveManager[F]] =
    (Mutex[F], Ref.of[F, Set[String]](Set.empty), Ref.of[F, Map[Int, Int]](Map.empty), Ref.of[F, Int](0))
      .mapN(
        new DaveManager(session, selfUserId, groupId, send, _, _, _, _)
      )
