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

package discord.gateway

import cats.effect.{Async, Deferred, Ref, Resource}
import cats.effect.std.{Queue, Random}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import discord.gateway.ws.{WsClient, WsConnection, WsMessage}
import discord.json.JsonObj
import discord.json.JsonObj.*
import discord.model.*
import discord.rest.DiscordRest
import fs2.Stream
import fs2.concurrent.{SignallingRef, Topic}
import fs2.io.net.Network
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.Uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

final case class GatewayConfig(
    token: String,
    intents: Intents,
    presence: Option[Presence] = None,
    /** `(shardId, shardCount)` */
    shard: Option[(Int, Int)] = None,
    largeThreshold: Int = 50,
    /** Events buffered per subscriber before the gateway waits for that subscriber. */
    bufferSize: Int = 1024,
    maxBackoff: FiniteDuration = 60.seconds
):
  override def toString: String = Redact.product(this, "token")

/** The gateway closed with a code that means reconnecting won't help (bad token, disallowed intents, ...). */
final case class GatewayClosed(code: Int, reason: String)
    extends RuntimeException(s"Gateway closed with code $code: $reason")

/** A connection to Discord's gateway. It identifies, sends heartbeats, and reconnects (resuming the session
  * when possible) by itself.
  */
trait Gateway[F[_]]:
  /** Dispatch events. Every call subscribes separately, and only sees events from that point on. The stream
    * fails if the gateway shuts down with a [[GatewayClosed]] error.
    */
  def events: Stream[F, GatewayEvent]

  /** Like [[events]], but the subscription is active as soon as the resource is acquired, so nothing
    * published after that point is missed. Use it when you send a command and then wait for the events it
    * causes.
    */
  def subscribe: Resource[F, Stream[F, GatewayEvent]]

  /** The most recent READY event. Waits for the first one. */
  def ready: F[GatewayEvent.Ready]

  /** Sends a command once the session is established. */
  def send(command: GatewayCommand): F[Unit]

  def updatePresence(presence: Presence): F[Unit] = send(GatewayCommand.UpdatePresence(presence))

  /** Joins, moves between or (with `channelId = None`) leaves voice channels. Usually you use the voice
    * module instead, which also opens the voice connection.
    */
  def updateVoiceState(
      guildId: Snowflake,
      channelId: Option[Snowflake],
      selfMute: Boolean = false,
      selfDeaf: Boolean = false
  ): F[Unit] = send(GatewayCommand.UpdateVoiceState(guildId, channelId, selfMute, selfDeaf))

object Gateway:
  /** Connects using the URL from `GET /gateway/bot`. */
  def apply[F[_]: Async: Network](
      rest: DiscordRest[F],
      config: GatewayConfig,
      logger: Option[Logger[F]] = None
  ): Resource[F, Gateway[F]] =
    Resource.eval(rest.getGatewayBot).flatMap(gb => connect(WsClient[F], gb.url, config, logger))

  /** Connects to `gatewayUrl` through `ws`. Useful for tests or custom transports. */
  def connect[F[_]: Async](
      ws: WsClient[F],
      gatewayUrl: String,
      config: GatewayConfig,
      logger: Option[Logger[F]] = None
  ): Resource[F, Gateway[F]] =
    val log = logger.getOrElse(NoOpLogger[F])
    for
      topic <- Resource.eval(Topic[F, GatewayEvent])
      outbound <- Resource.eval(Queue.unbounded[F, GatewayPayload])
      session <- Resource.eval(Ref.of[F, Session](Session.Empty))
      readySignal <- Resource.eval(SignallingRef.of[F, Option[GatewayEvent.Ready]](None))
      outcome <- Resource.eval(Deferred[F, Either[Throwable, Unit]])
      random <- Resource.eval(Random.scalaUtilRandom[F])
      runner = Runner(ws, gatewayUrl, config, log, topic, outbound, session, readySignal, random)
      // Record the outcome before closing the topic, so subscribers see the error when their stream ends.
      _ <- runner.loop(0).attempt.flatMap(r => outcome.complete(r) >> topic.close).background
      failIfFailed = Stream.exec(outcome.tryGet.flatMap(_.traverse_(Async[F].fromEither)))
    yield new Gateway[F]:
      def events: Stream[F, GatewayEvent] = topic.subscribe(config.bufferSize) ++ failIfFailed
      def subscribe: Resource[F, Stream[F, GatewayEvent]] =
        topic.subscribeAwait(config.bufferSize).map(_ ++ failIfFailed)
      def ready: F[GatewayEvent.Ready] =
        readySignal.discrete.unNone.head.compile.lastOrError
      def send(command: GatewayCommand): F[Unit] = outbound.offer(command.toPayload)

  private final case class Session(sessionId: Option[String], resumeUrl: Option[String], seq: Option[Long]):
    def canResume: Boolean = sessionId.isDefined
  private object Session:
    val Empty: Session = Session(None, None, None)

  private enum Next:
    /** Open a new connection, resuming the session if `resume`. `healthy` if this one got to READY/RESUMED.
      */
    case Reconnect(resume: Boolean, healthy: Boolean, delay: FiniteDuration = Duration.Zero)
    case Fatal(error: Throwable)

  private val FatalCloseCodes = Set(4004, 4010, 4011, 4012, 4013, 4014)
  private val NonResumableCloseCodes = Set(4007, 4009)

  /** Closing with 1000/1001 invalidates the session, so we close with this when we intend to resume. */
  private val ResumeCloseCode = 4900

  private final case class Runner[F[_]](
      ws: WsClient[F],
      gatewayUrl: String,
      config: GatewayConfig,
      log: Logger[F],
      topic: Topic[F, GatewayEvent],
      outbound: Queue[F, GatewayPayload],
      session: Ref[F, Session],
      readySignal: SignallingRef[F, Option[GatewayEvent.Ready]],
      random: Random[F]
  )(using F: Async[F]):

    def loop(failures: Int): F[Unit] =
      session.get.flatMap { s =>
        val url = if s.canResume then s.resumeUrl.getOrElse(gatewayUrl) else gatewayUrl
        connectOnce(url, s.canResume)
          .handleErrorWith(e =>
            log.warn(e)("Gateway connection failed").as(Next.Reconnect(resume = true, healthy = false))
          )
          .flatMap {
            case Next.Fatal(e) => log.error(e)("Gateway closed permanently") >> F.raiseError(e)
            case Next.Reconnect(resume, healthy, delay) =>
              val nextFailures = if healthy then 0 else failures + 1
              for
                _ <- session.set(Session.Empty).unlessA(resume)
                backoff <- if healthy then F.pure(delay) else backoffFor(nextFailures).map(_.max(delay))
                _ <- log.info(s"Reconnecting to the gateway in $backoff (resume=$resume)")
                _ <- F.sleep(backoff)
                _ <- loop(nextFailures)
              yield ()
          }
      }

    private def backoffFor(failures: Int): F[FiniteDuration] =
      val base = (1L << failures.min(6)).seconds.min(config.maxBackoff)
      random.betweenDouble(0.5, 1.0).map(j => (base.toMillis * j).toLong.millis)

    private def connectOnce(url: String, resume: Boolean): F[Next] =
      F.fromEither(Uri.fromString(url))
        .map(_.withQueryParam("v", 10).withQueryParam("encoding", "json"))
        .flatMap(uri => ws.connect(uri).use(runSession(_, resume)))

    private def runSession(conn: WsConnection[F], resume: Boolean): F[Next] =
      for
        done <- Deferred[F, Next]
        acked <- Ref.of[F, Boolean](true)
        established <- Deferred[F, Unit]
        interval <- Deferred[F, FiniteDuration]
        ctx = SessionCtx(conn, resume, done, acked, established, interval)
        main = conn.receive.evalMap(ctx.handle)
        heartbeats = Stream.eval(interval.get).flatMap(ctx.heartbeatLoop)
        commands = Stream
          .exec(established.get) ++ Stream.fromQueueUnterminated(outbound).evalMap(ctx.sendPayload)
        _ <- main
          .concurrently(heartbeats)
          .concurrently(commands)
          .interruptWhen(done.get.as(Either.unit[Throwable]))
          .compile
          .drain
        healthy <- established.tryGet.map(_.isDefined)
        next <- done.tryGet.map(_.getOrElse(Next.Reconnect(resume = true, healthy)))
        // Close without invalidating the session when we're going to resume.
        _ <- next match
          case Next.Reconnect(true, _, _) => conn.close(ResumeCloseCode, "resuming").attempt.void
          case _                          => F.unit
      yield next

    private final case class SessionCtx(
        conn: WsConnection[F],
        resume: Boolean,
        done: Deferred[F, Next],
        acked: Ref[F, Boolean],
        established: Deferred[F, Unit],
        interval: Deferred[F, FiniteDuration]
    ):
      def sendPayload(p: GatewayPayload): F[Unit] = conn.sendText(p.asJson.noSpaces)

      private def healthy: F[Boolean] = established.tryGet.map(_.isDefined)

      private def finish(next: F[Next]): F[Unit] = next.flatMap(done.complete).void

      def heartbeat: F[Unit] =
        session.get.flatMap(s => sendPayload(GatewayPayload(Opcode.Heartbeat, s.seq.asJson)))

      def heartbeatLoop(every: FiniteDuration): Stream[F, Unit] =
        val beat = acked.getAndSet(false).flatMap { ok =>
          if ok then heartbeat
          else log.warn("No heartbeat ACK, reconnecting") >> finish(healthy.map(Next.Reconnect(true, _)))
        }
        Stream.exec(random.nextDouble.flatMap(j => F.sleep((every.toMillis * j).toLong.millis))) ++
          Stream.repeatEval(beat >> F.sleep(every))

      def handle(msg: WsMessage): F[Unit] = msg match
        case WsMessage.Text(text) =>
          F.fromEither(parse(text).flatMap(_.as[GatewayPayload])).flatMap(onPayload)
        case WsMessage.Binary(_)           => log.warn("Ignoring binary gateway message")
        case WsMessage.Close(code, reason) =>
          log.info(s"Gateway closed the connection: ${code.getOrElse("-")} $reason") >>
            finish(code match
              case Some(c) if FatalCloseCodes(c)        => F.pure(Next.Fatal(GatewayClosed(c, reason)))
              case Some(c) if NonResumableCloseCodes(c) => healthy.map(Next.Reconnect(false, _))
              case _                                    => healthy.map(Next.Reconnect(true, _)))

      private def onPayload(p: GatewayPayload): F[Unit] = p.op match
        case Opcode.Hello =>
          val every = p.d.hcursor.get[Long]("heartbeat_interval").getOrElse(41250L).millis
          interval.complete(every) >> session.get.flatMap { s =>
            if resume && s.canResume then sendPayload(resumePayload(s)) else sendPayload(identifyPayload)
          }
        case Opcode.HeartbeatAck => acked.set(true)
        case Opcode.Heartbeat    => heartbeat
        case Opcode.Reconnect    =>
          log.info("Gateway asked us to reconnect") >> finish(healthy.map(Next.Reconnect(true, _)))
        case Opcode.InvalidSession =>
          val resumable = p.d.asBoolean.getOrElse(false)
          random.betweenLong(1000, 5000).flatMap { ms =>
            log.info(s"Invalid session (resumable=$resumable)") >>
              finish(F.pure(Next.Reconnect(resumable, healthy = true, delay = ms.millis)))
          }
        case Opcode.Dispatch =>
          session.update(s => s.copy(seq = p.s.orElse(s.seq))) >>
            p.t.traverse_(name => dispatch(GatewayEvent.decode(name, p.d)))
        case other => log.debug(s"Ignoring gateway opcode $other")

      private def dispatch(event: GatewayEvent): F[Unit] =
        val bookkeeping = event match
          case r: GatewayEvent.Ready =>
            session.update(_.copy(sessionId = Some(r.sessionId), resumeUrl = Some(r.resumeGatewayUrl))) >>
              readySignal.set(Some(r)) >> established.complete(()).void >> log.info("Gateway session ready")
          case GatewayEvent.Resumed => established.complete(()).void >> log.info("Gateway session resumed")
          case GatewayEvent.Unknown(name, _, Some(err)) => log.warn(s"Could not decode $name event: $err")
          case _                                        => F.unit
        bookkeeping >> topic.publish1(event).void

    private def identifyPayload: GatewayPayload =
      val os = Option(System.getProperty("os.name")).getOrElse("unknown")
      GatewayPayload(
        Opcode.Identify,
        JsonObj(
          "token" := config.token,
          "intents" := config.intents,
          "properties" := Json.obj("os" := os, "browser" := "discord-scala", "device" := "discord-scala"),
          "large_threshold" := config.largeThreshold,
          "shard" :=? config.shard.map((id, count) => List(id, count)),
          "presence" :=? config.presence
        )
      )

    private def resumePayload(s: Session): GatewayPayload =
      GatewayPayload(
        Opcode.Resume,
        Json.obj("token" := config.token, "session_id" := s.sessionId, "seq" := s.seq)
      )
