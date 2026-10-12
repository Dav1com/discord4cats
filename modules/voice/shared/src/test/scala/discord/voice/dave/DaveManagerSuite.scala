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

import cats.effect.{IO, Ref}
import munit.CatsEffectSuite
import scodec.bits.*

import scala.collection.mutable

class DaveManagerSuite extends CatsEffectSuite:

  /** Records calls instead of doing MLS. */
  private final class FakeSession extends DaveSession:
    val calls = mutable.ListBuffer.empty[String]
    var commitOutcome: CommitOutcome = CommitOutcome.Joined
    var version = 0
    def maxProtocolVersion = 1
    def init(version: Int, groupId: Long, selfUserId: String) =
      this.version = version
      calls += s"init($version,$groupId,$selfUserId)"
    def reset() = calls += "reset"
    def protocolVersion = version
    def setExternalSender(data: ByteVector) = calls += s"externalSender(${data.toHex})"
    def keyPackage() = hex"4b50"
    def processProposals(payload: ByteVector, ids: Set[String]) =
      calls += s"proposals(${payload.toHex},${ids.toList.sorted.mkString("+")})"
      Some(hex"c0de")
    def processCommit(commit: ByteVector) =
      calls += s"commit(${commit.toHex})"
      commitOutcome
    def processWelcome(welcome: ByteVector, ids: Set[String]) =
      calls += s"welcome(${welcome.toHex})"
      true
    def useSelfKeyRatchet(version: Int) = calls += s"ratchet($version)"
    def assignOpusSsrc(ssrc: Int) = ()
    def encryptOpus(ssrc: Int, frame: ByteVector) = Some(frame)
    def close() = ()

  private def setup =
    for
      sent <- Ref.of[IO, List[DaveOutbound]](Nil)
      session = FakeSession()
      manager <- DaveManager[IO](session, "1", 42L, o => sent.update(_ :+ o))
    yield (session, manager, sent)

  test("DAVE v1 session: init the group and send a key package") {
    setup.flatMap { (session, manager, sent) =>
      manager.onSessionDescription(1) >> sent.get.map { out =>
        assertEquals(session.calls.toList, List("init(1,42,1)"))
        assertEquals(out, List(DaveOutbound.KeyPackage(hex"4b50")))
      }
    }
  }

  test("DAVE disabled (stage channels): passthrough, no MLS") {
    setup.flatMap { (session, manager, sent) =>
      manager.onSessionDescription(0) >> sent.get.map { out =>
        assertEquals(session.calls.toList, List("ratchet(0)"))
        assertEquals(out, Nil)
      }
    }
  }

  test("proposals are checked against connected users and answered with a commit") {
    setup.flatMap { (session, manager, sent) =>
      manager.onClientsConnect(List("7", "8")) >> manager.onClientDisconnect("8") >>
        manager.onProposals(hex"00ff") >> sent.get.map { out =>
          assertEquals(session.calls.toList, List("proposals(00ff,1+7)"))
          assertEquals(out, List(DaveOutbound.CommitWelcome(hex"c0de")))
        }
    }
  }

  test("an announced commit is acknowledged, and the new key is only used on execute") {
    setup.flatMap { (session, manager, sent) =>
      for
        _ <- manager.onSessionDescription(1)
        _ <- manager.onAnnounceCommit(3, hex"cc")
        before <- IO(session.calls.toList)
        _ <- manager.onExecuteTransition(3)
        out <- sent.get
      yield
        assertEquals(before, List("init(1,42,1)", "commit(cc)"))
        assertEquals(session.calls.toList.last, "ratchet(1)")
        assertEquals(out.last, DaveOutbound.TransitionReady(3))
    }
  }

  test("a commit that fails is reported and the group is re-initialized") {
    setup.flatMap { (session, manager, sent) =>
      session.commitOutcome = CommitOutcome.Failed
      for
        _ <- manager.onSessionDescription(1)
        _ <- manager.onAnnounceCommit(4, hex"cc")
        out <- sent.get
      yield
        assertEquals(
          out.drop(1),
          List(DaveOutbound.InvalidCommitWelcome(4), DaveOutbound.KeyPackage(hex"4b50"))
        )
        assertEquals(session.calls.toList.last, "init(1,42,1)")
    }
  }

  test("transition id 0 applies immediately, without a ready message") {
    setup.flatMap { (session, manager, sent) =>
      manager.onWelcome(0, hex"ee") >> sent.get.map { out =>
        assertEquals(session.calls.toList, List("welcome(ee)", "ratchet(0)"))
        assertEquals(out, Nil)
      }
    }
  }
