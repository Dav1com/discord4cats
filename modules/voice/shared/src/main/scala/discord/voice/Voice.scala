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

import cats.effect.{Async, Deferred, LiftIO, Ref, Resource}
import cats.effect.std.Mutex
import cats.effect.syntax.all.*
import cats.syntax.all.*
import discord.gateway.{Gateway, GatewayEvent}
import discord.gateway.ws.{WsClient, WsConnection, WsMessage}
import discord.model.{Redact, Snowflake}
import discord.voice.dave.{DaveManager, DaveOutbound, DaveSession}
import fs2.Stream
import fs2.io.net.Network
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.Uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

/** A connection to a voice channel that can send audio. */
trait VoiceConnection[F[_]]:
  def guildId: Snowflake
  def channelId: Snowflake

  /** Streams Opus frames to the channel at real-time pace (one per 20ms), followed by a few silence frames.
    * Only one `play` runs at a time; a second call waits for the first. Cancel the returned effect to stop.
    */
  def play(frames: Stream[F, OpusFrame]): F[Unit]

  /** The negotiated DAVE protocol version. 0 means the call isn't end-to-end encrypted (e.g. stage channels).
    */
  def daveProtocolVersion: F[Int]

  /** Completes when the voice connection ends for good (e.g. the bot was kicked from the channel). */
  def closed: F[Unit]

final case class VoiceClosed(code: Option[Int], reason: String)
    extends RuntimeException(s"Voice connection closed${code.fold("")(c => s" with code $c")}: $reason")

object Voice:

  /** Joins a voice channel through the main gateway and opens the voice connection. Leaves the channel when
    * the resource is released.
    *
    * Needs the `GuildVoiceStates` intent.
    */
  def join[F[_]: Async: Network: LiftIO](
      gateway: Gateway[F],
      guildId: Snowflake,
      channelId: Snowflake,
      selfMute: Boolean = false,
      selfDeaf: Boolean = true,
      timeout: FiniteDuration = 10.seconds,
      logger: Option[Logger[F]] = None
  ): Resource[F, VoiceConnection[F]] =
    val log = logger.getOrElse(NoOpLogger[F])
    for
      ready <- Resource.eval(gateway.ready)
      events <- gateway.subscribe
      _ <- Resource.make(gateway.updateVoiceState(guildId, Some(channelId), selfMute, selfDeaf))(_ =>
        gateway.updateVoiceState(guildId, None).attempt.void
      )
      info <- Resource.eval(awaitServer(events, guildId, ready.user.id).timeout(timeout))
      conn <- connect(WsClient[F], info, guildId, channelId, ready.user.id, log)
    yield conn

  /** What the main gateway tells us about our voice session. */
  final case class ServerInfo(sessionId: String, token: String, endpoint: String):
    override def toString: String = Redact.product(this, "token")

  private def awaitServer[F[_]: Async](
      events: Stream[F, GatewayEvent],
      guildId: Snowflake,
      selfId: Snowflake
  ): F[ServerInfo] =
    events
      .scan((Option.empty[String], Option.empty[(String, String)])) {
        case ((_, server), GatewayEvent.VoiceStateUpdate(s))
            if s.userId == selfId && s.guildId.contains(guildId) =>
          (Some(s.sessionId), server)
        case ((session, _), GatewayEvent.VoiceServerUpdate(token, g, Some(endpoint))) if g == guildId =>
          (session, Some(token -> endpoint))
        case (acc, _) => acc
      }
      .collectFirst { case (Some(session), Some((token, endpoint))) => ServerInfo(session, token, endpoint) }
      .compile
      .lastOrError

  private object Op:
    val Identify = 0
    val SelectProtocol = 1
    val Ready = 2
    val Heartbeat = 3
    val SessionDescription = 4
    val Speaking = 5
    val HeartbeatAck = 6
    val Resume = 7
    val Hello = 8
    val Resumed = 9
    val ClientsConnect = 11
    val ClientDisconnect = 13
    val DavePrepareTransition = 21
    val DaveExecuteTransition = 22
    val DaveTransitionReady = 23
    val DavePrepareEpoch = 24
    val DaveExternalSender = 25
    val DaveKeyPackage = 26
    val DaveProposals = 27
    val DaveCommitWelcome = 28
    val DaveAnnounceCommit = 29
    val DaveWelcome = 30
    val DaveInvalidCommitWelcome = 31

  /** Close codes after which resuming is pointless. */
  private val TerminalCloseCodes = Set(4003, 4004, 4006, 4011, 4012, 4014, 4016, 4022)
  private val MaxResumeAttempts = 5

  private final case class ReadyInfo(ssrc: Int, ip: String, port: Int, modes: List[String])

  private given Decoder[ReadyInfo] = Decoder.forProduct4("ssrc", "ip", "port", "modes")(ReadyInfo.apply)

  private final case class SessionInfo(secretKey: ByteVector, daveVersion: Int):
    override def toString: String = Redact.product(this, "secretKey")

  /** Opens the voice connection described by `info`. Exposed for custom setups and tests. */
  def connect[F[_]: Async: Network: LiftIO](
      ws: WsClient[F],
      info: ServerInfo,
      guildId: Snowflake,
      channelId: Snowflake,
      selfId: Snowflake,
      log: Logger[F],
      openUdp: Option[(String, Int) => Resource[F, UdpSocket[F]]] = None
  ): Resource[F, VoiceConnection[F]] =
    val F = Async[F]
    val endpoint = if info.endpoint.contains("://") then info.endpoint else s"wss://${info.endpoint}"
    for
      uri <- Resource.eval(F.fromEither(Uri.fromString(endpoint)).map(_.withQueryParam("v", 8)))
      current <- Resource.eval(Ref.of[F, Option[WsConnection[F]]](None))
      seq <- Resource.eval(Ref.of[F, Int](-1))
      readyD <- Resource.eval(Deferred[F, ReadyInfo])
      sessionD <- Resource.eval(Deferred[F, SessionInfo])
      closedD <- Resource.eval(Deferred[F, Either[Throwable, Unit]])
      daveSession <- Resource.make(F.delay(DaveSession.create()))(s => F.delay(s.close()))

      sendJson = (op: Int, d: Json) =>
        current.get.flatMap(_.traverse_(_.sendText(Json.obj("op" := op, "d" := d).noSpaces)))
      sendBinary = (op: Int, payload: ByteVector) =>
        current.get.flatMap(_.traverse_(_.send(WsMessage.Binary(ByteVector(op.toByte) ++ payload))))

      dave <- Resource.eval(
        DaveManager[F](
          daveSession,
          selfId.asString,
          channelId.value,
          {
            case DaveOutbound.KeyPackage(kp)     => sendBinary(Op.DaveKeyPackage, kp)
            case DaveOutbound.CommitWelcome(cw)  => sendBinary(Op.DaveCommitWelcome, cw)
            case DaveOutbound.TransitionReady(t) =>
              sendJson(Op.DaveTransitionReady, Json.obj("transition_id" := t))
            case DaveOutbound.InvalidCommitWelcome(t) =>
              sendJson(Op.DaveInvalidCommitWelcome, Json.obj("transition_id" := t))
          }
        )
      )

      identify = sendJson(
        Op.Identify,
        Json.obj(
          "server_id" := guildId,
          "user_id" := selfId,
          "session_id" := info.sessionId,
          "token" := info.token,
          "max_dave_protocol_version" := daveSession.maxProtocolVersion
        )
      )
      resume = seq.get.flatMap(s =>
        sendJson(
          Op.Resume,
          Json.obj(
            "server_id" := guildId,
            "session_id" := info.sessionId,
            "token" := info.token,
            "seq_ack" := s
          )
        )
      )

      handleText = (text: String) =>
        F.fromEither(parse(text)).flatMap { json =>
          val c = json.hcursor
          val d = c.downField("d")
          val op = c.get[Int]("op").getOrElse(-1)
          log.debug(s"Voice gateway op $op: ${d.focus.fold("")(Redact.json(_).noSpaces.take(200))}") >>
            c.get[Option[Int]]("seq").toOption.flatten.traverse_(seq.set) >>
            (op match
              case Op.Ready              => F.fromEither(d.as[ReadyInfo]).flatMap(readyD.complete).void
              case Op.SessionDescription =>
                for
                  key <- F.fromEither(d.get[List[Int]]("secret_key"))
                  version = d.get[Int]("dave_protocol_version").getOrElse(0)
                  _ <- dave.onSessionDescription(version)
                  _ <- sessionD.complete(SessionInfo(ByteVector(key.map(_.toByte)), version))
                yield ()
              case Op.ClientsConnect => dave.onClientsConnect(d.get[List[String]]("user_ids").getOrElse(Nil))
              case Op.ClientDisconnect => d.get[String]("user_id").toOption.traverse_(dave.onClientDisconnect)
              case Op.DavePrepareTransition =>
                dave.onPrepareTransition(
                  d.get[Int]("transition_id").getOrElse(0),
                  d.get[Int]("protocol_version").getOrElse(0)
                )
              case Op.DaveExecuteTransition =>
                dave.onExecuteTransition(d.get[Int]("transition_id").getOrElse(0))
              case Op.DavePrepareEpoch =>
                dave.onPrepareEpoch(
                  d.get[Long]("epoch").getOrElse(0L),
                  d.get[Int]("protocol_version").getOrElse(0)
                )
              case Op.Resumed => log.info("Voice session resumed")
              case _          => F.unit)
        }

      handleBinary = (data: ByteVector) =>
        if data.size < 3 then F.unit
        else
          val op = data(2) & 0xff
          val payload = data.drop(3)
          log.debug(s"Voice gateway binary op $op (${payload.size} bytes)") >>
            seq.set(data.take(2).toInt(signed = false)) >> (op match
              case Op.DaveExternalSender => dave.onExternalSender(payload)
              case Op.DaveProposals      => dave.onProposals(payload)
              case Op.DaveAnnounceCommit =>
                dave.onAnnounceCommit(payload.take(2).toInt(signed = false), payload.drop(2))
              case Op.DaveWelcome => dave.onWelcome(payload.take(2).toInt(signed = false), payload.drop(2))
              case other          => log.debug(s"Ignoring binary voice opcode $other"))

      /** Runs one WebSocket connection. Returns the close frame, if the server sent one. */
      runOnce = (resuming: Boolean) =>
        ws.connect(uri).use { conn =>
          for
            interval <- Deferred[F, FiniteDuration]
            _ <- current.set(Some(conn))
            _ <- if resuming then resume else identify
            heartbeats = Stream.eval(interval.get).flatMap { every =>
              Stream.repeatEval(
                (F.realTime, seq.get).flatMapN((now, s) =>
                  sendJson(Op.Heartbeat, Json.obj("t" := now.toMillis, "seq_ack" := s))
                ) >> F.sleep(every)
              )
            }
            closeFrame <- conn.receive
              .evalMap {
                case WsMessage.Text(text) =>
                  val hello =
                    parse(text).toOption.flatMap(
                      _.hcursor.downField("d").get[Double]("heartbeat_interval").toOption
                    )
                  hello
                    .fold(handleText(text).handleErrorWith(e => log.warn(e)("Bad voice gateway message")))(
                      ms => interval.complete(ms.toLong.millis).void
                    )
                    .as(None)
                case WsMessage.Binary(data) =>
                  handleBinary(data)
                    .handleErrorWith(e => log.warn(e)("Bad binary voice gateway message"))
                    .as(None)
                case close: WsMessage.Close => F.pure(Some(close))
              }
              .unNone
              .concurrently(heartbeats)
              .compile
              .last
            _ <- current.set(None)
          yield closeFrame
        }

      /** Keeps the WebSocket alive, resuming after transient disconnects. */
      driver = {
        def loop(resuming: Boolean, attempts: Int): F[Unit] =
          runOnce(resuming).attempt.flatMap {
            case Right(Some(WsMessage.Close(Some(code), reason))) if TerminalCloseCodes(code) =>
              F.raiseError(VoiceClosed(Some(code), reason))
            case result if attempts >= MaxResumeAttempts =>
              F.raiseError(VoiceClosed(None, s"Giving up after $attempts reconnects: $result"))
            case result =>
              log.info(s"Voice connection dropped ($result), resuming") >>
                F.sleep((attempts + 1).seconds) >> loop(resuming = true, attempts + 1)
          }
        loop(resuming = false, 0)
      }
      _ <- driver.attempt.flatMap(closedD.complete).background

      failIfClosed = closedD.get.flatMap {
        case Left(e)  => F.raiseError[Unit](e)
        case Right(_) => F.raiseError[Unit](VoiceClosed(None, "Voice connection ended"))
      }
      readyInfo <- Resource.eval(F.race(failIfClosed, readyD.get).map(_.merge.asInstanceOf[ReadyInfo]))
      _ <- Resource.eval(
        F.raiseUnless(readyInfo.modes.contains(VoicePacket.Mode))(
          VoiceClosed(None, s"Voice server doesn't offer ${VoicePacket.Mode} (offered: ${readyInfo.modes})")
        )
      )
      udp <- openUdp.getOrElse(UdpSocket.connect[F])(readyInfo.ip, readyInfo.port)
      external <- Resource.eval(discoverIp(udp, readyInfo.ssrc).timeout(5.seconds))
      _ <- Resource.eval(dave.assignOpusSsrc(readyInfo.ssrc))
      _ <- Resource.eval(
        sendJson(
          Op.SelectProtocol,
          Json.obj(
            "protocol" := "udp",
            "data" := Json.obj("address" := external._1, "port" := external._2, "mode" := VoicePacket.Mode)
          )
        )
      )
      session <- Resource.eval(F.race(failIfClosed, sessionD.get).map(_.merge.asInstanceOf[SessionInfo]))
      _ <- Resource.eval(log.info(s"Voice connected (ssrc ${readyInfo.ssrc}, DAVE v${session.daveVersion})"))
      cipher = AesGcm(session.secretKey)
      rtp <- Resource.eval(Ref.of[F, RtpState](RtpState.initial(readyInfo.ssrc)))
      nonce <- Resource.eval(Ref.of[F, Int](0))
      playLock <- Resource.eval(Mutex[F])
      // (sent, dropped) frames, for diagnostics.
      counters <- Resource.eval(Ref.of[F, (Long, Long)]((0L, 0L)))
    yield
      def speaking(on: Boolean): F[Unit] =
        sendJson(
          Op.Speaking,
          Json.obj("speaking" := (if on then 1 else 0), "delay" := 0, "ssrc" := readyInfo.ssrc)
        )

      def sendFrame(frame: OpusFrame): F[Unit] =
        val e2ee =
          if frame.bytes == OpusFrame.Silence.bytes then F.pure(Some(frame.bytes))
          else dave.encryptOpus(readyInfo.ssrc, frame.bytes)
        (rtp.getAndUpdate(_.next), nonce.getAndUpdate(_ + 1), e2ee).flatMapN { (state, counter, payload) =>
          payload match
            case Some(p) =>
              udp.send(VoicePacket.encrypt(cipher, state, counter, p)) >> counters.update((s, d) =>
                (s + 1, d)
              )
            case None => counters.update((s, d) => (s, d + 1))
        }

      /** One frame per 20ms. `immediately` sends the first frame now instead of one period from now. */
      def paced(frames: Stream[F, OpusFrame], immediately: Boolean): Stream[F, Unit] =
        val ticks = if immediately then Stream.fixedRateStartImmediately[F](20.millis)
        else Stream.fixedRate[F](20.millis)
        frames.zipLeft(ticks).evalMap(sendFrame)

      val (gId, cId) = (guildId, channelId)
      new VoiceConnection[F]:
        def guildId: Snowflake = gId
        def channelId: Snowflake = cId

        def play(frames: Stream[F, OpusFrame]): F[Unit] =
          val report = counters.getAndSet((0L, 0L)).flatMap { (sent, dropped) =>
            log.info(s"Played $sent frames" + (if dropped > 0 then s", dropped $dropped (no DAVE key yet)"
                                               else ""))
          }
          playLock.lock.surround(
            (speaking(true) >> paced(frames, immediately = true).compile.drain)
              .guarantee(report)
              .guarantee(
                // Continues the 20ms cadence of the last audio frame.
                paced(
                  Stream.emits(List.fill(5)(OpusFrame.Silence)),
                  immediately = false
                ).compile.drain.attempt.void >>
                  speaking(false).attempt.void
              )
          )

        def daveProtocolVersion: F[Int] = dave.protocolVersion

        def closed: F[Unit] = closedD.get.void

  /** Discord's IP discovery: asks the voice server which public address and port our packets come from. */
  private def discoverIp[F[_]: Async](udp: UdpSocket[F], ssrc: Int): F[(String, Int)] =
    val request =
      ByteVector.fromInt(1, 2) ++ ByteVector.fromInt(70, 2) ++ ByteVector.fromInt(ssrc) ++ ByteVector.low(66)
    udp.send(request) >> udp.receive.flatMap { resp =>
      if resp.size < 74 || resp.take(2).toInt(signed = false) != 2 then
        Async[F].raiseError(VoiceClosed(None, "Invalid IP discovery response"))
      else
        val address = new String(resp.slice(8, 72).toArray.takeWhile(_ != 0), StandardCharsets.US_ASCII)
        Async[F].pure(address -> resp.slice(72, 74).toInt(signed = false))
    }
