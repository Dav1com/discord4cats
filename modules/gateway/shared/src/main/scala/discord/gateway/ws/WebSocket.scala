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

package discord.gateway.ws

import cats.effect.{Async, Ref, Resource}
import cats.effect.std.{Mutex, SecureRandom}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port, SocketAddress}
import fs2.{Chunk, Pull, Stream}
import fs2.io.net.{Network, Socket}
import org.http4s.Uri
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets

/** A message received from, or sent to, a WebSocket. Fragmented messages are reassembled. */
enum WsMessage:
  case Text(data: String)
  case Binary(data: ByteVector)

  /** The peer closed the connection. `code` is absent when the close frame had no status. */
  case Close(code: Option[Int], reason: String)

/** An open WebSocket connection. */
trait WsConnection[F[_]]:
  /** Incoming messages. Pings are answered automatically. Ends after a close frame or when the socket closes.
    * Consume it only once.
    */
  def receive: Stream[F, WsMessage]
  def send(message: WsMessage): F[Unit]
  def sendText(text: String): F[Unit] = send(WsMessage.Text(text))
  def close(code: Int, reason: String = ""): F[Unit] = send(WsMessage.Close(Some(code), reason))

/** Opens WebSocket connections. */
trait WsClient[F[_]]:
  def connect(uri: Uri): Resource[F, WsConnection[F]]

final class WebSocketException(message: String) extends RuntimeException(message)

object WsClient:
  private val Guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  private val MaxHeaderBytes = 16 * 1024

  /** A minimal RFC 6455 client over fs2-io sockets. It runs unchanged on the JVM, Node.js and Scala Native.
    */
  def apply[F[_]: Async: Network]: WsClient[F] = new WsClient[F]:
    def connect(uri: Uri): Resource[F, WsConnection[F]] =
      for
        secure <- Resource.eval(uri.scheme.map(_.value.toLowerCase) match
          case Some("wss" | "https") => Async[F].pure(true)
          case Some("ws" | "http")   => Async[F].pure(false)
          case other => Async[F].raiseError(WebSocketException(s"Unsupported WebSocket scheme: $other")))
        hostStr <- Resource.eval(
          Async[F].fromOption(uri.host.map(_.value), WebSocketException(s"No host in $uri"))
        )
        host <- Resource.eval(
          Async[F].fromOption(Host.fromString(hostStr), WebSocketException(s"Invalid host: $hostStr"))
        )
        port = uri.port
          .flatMap(Port.fromInt)
          .getOrElse(if secure then Port.fromInt(443).get else Port.fromInt(80).get)
        raw <- Network[F].connect(SocketAddress(host, port))
        socket <-
          if secure then
            Network[F].tlsContext.systemResource.flatMap(
              _.clientBuilder(raw).withParameters(TlsPlatform.clientParameters(hostStr)).build
            )
          else Resource.pure[F, Socket[F]](raw)
        random <- Resource.eval(SecureRandom.javaSecuritySecureRandom[F])
        leftover <- Resource.eval(handshake(socket, uri, hostStr, port, secure, random))
        conn <- Resource.make(Connection.create(socket, leftover, random))(_.closeQuietly)
      yield conn

  private def handshake[F[_]](
      socket: Socket[F],
      uri: Uri,
      host: String,
      port: Port,
      secure: Boolean,
      random: SecureRandom[F]
  )(using F: Async[F]): F[ByteVector] =
    val path = uri.copy(scheme = None, authority = None, fragment = None).renderString match
      case "" => "/"
      case p  => if p.startsWith("/") then p else "/" + p
    val defaultPort = if secure then 443 else 80
    val hostHeader = if port.value == defaultPort then host else s"$host:${port.value}"
    for
      keyBytes <- random.nextBytes(16)
      key = ByteVector.view(keyBytes).toBase64
      request =
        s"GET $path HTTP/1.1\r\n" +
          s"Host: $hostHeader\r\n" +
          "Upgrade: websocket\r\n" +
          "Connection: Upgrade\r\n" +
          s"Sec-WebSocket-Key: $key\r\n" +
          "Sec-WebSocket-Version: 13\r\n" +
          "\r\n"
      _ <- socket.write(Chunk.array(request.getBytes(StandardCharsets.US_ASCII)))
      response <- readHead(socket, ByteVector.empty)
      (head, rest) = response
      lines = head.split("\r\n").toList
      status = lines.headOption.getOrElse("")
      _ <- F.raiseUnless(status.split(' ').lift(1).contains("101"))(
        WebSocketException(s"WebSocket upgrade to $uri failed: $status")
      )
      headers = lines
        .drop(1)
        .flatMap { l =>
          l.indexOf(':') match
            case -1 => None
            case i  => Some(l.take(i).trim.toLowerCase -> l.drop(i + 1).trim)
        }
        .toMap
      expected = Sha1(ByteVector.view((key + Guid).getBytes(StandardCharsets.US_ASCII)))
      _ <- F.raiseUnless(headers.get("sec-websocket-accept").contains(expected.toBase64))(
        WebSocketException("Invalid Sec-WebSocket-Accept in upgrade response")
      )
    yield rest

  /** Reads until the end of the HTTP response head. Returns the head and any bytes received after it. */
  private def readHead[F[_]](socket: Socket[F], acc: ByteVector)(using F: Async[F]): F[(String, ByteVector)] =
    val end = acc.indexOfSlice(ByteVector('\r', '\n', '\r', '\n'))
    if end >= 0 then
      F.pure(new String(acc.take(end).toArray, StandardCharsets.ISO_8859_1) -> acc.drop(end + 4))
    else if acc.size > MaxHeaderBytes then
      F.raiseError(WebSocketException("WebSocket upgrade response too large"))
    else
      socket.read(8192).flatMap {
        case Some(chunk) => readHead(socket, acc ++ chunk.toByteVector)
        case None        => F.raiseError(WebSocketException("Connection closed during WebSocket upgrade"))
      }

  private final class Connection[F[_]](
      socket: Socket[F],
      leftover: ByteVector,
      random: SecureRandom[F],
      writeLock: Mutex[F],
      closeSent: Ref[F, Boolean]
  )(using F: Async[F])
      extends WsConnection[F]:

    private def write(frame: Frame): F[Unit] =
      random.nextBytes(4).flatMap { mask =>
        writeLock.lock.surround(
          socket.write(Chunk.byteVector(Frame.encode(frame, Some(ByteVector.view(mask)))))
        )
      }

    def send(message: WsMessage): F[Unit] = message match
      case WsMessage.Text(s) =>
        write(Frame(true, Frame.Text, ByteVector.view(s.getBytes(StandardCharsets.UTF_8))))
      case WsMessage.Binary(b)           => write(Frame(true, Frame.Binary, b))
      case WsMessage.Close(code, reason) =>
        val payload = code.fold(ByteVector.empty)(c =>
          ByteVector.fromInt(c, 2) ++ ByteVector.view(reason.getBytes(StandardCharsets.UTF_8)).take(123)
        )
        // Only the first close frame goes out.
        closeSent.getAndSet(true).flatMap(sent => write(Frame(true, Frame.Close, payload)).unlessA(sent))

    def closeQuietly: F[Unit] = send(WsMessage.Close(Some(1000), "")).attempt.void

    def receive: Stream[F, WsMessage] =
      (Stream.chunk(Chunk.byteVector(leftover)) ++ socket.reads)
        .through(frames)
        .through(messages)

    private def frames(in: Stream[F, Byte]): Stream[F, Frame] =
      def go(s: Stream[F, Byte], buf: ByteVector): Pull[F, Frame, Unit] =
        Frame.decode(buf) match
          case Left(err)                  => Pull.raiseError[F](WebSocketException(err))
          case Right(Some((frame, rest))) => Pull.output1(frame) >> go(s, rest)
          case Right(None)                =>
            s.pull.uncons.flatMap {
              case Some((chunk, tail)) => go(tail, buf ++ chunk.toByteVector)
              case None                => Pull.done
            }
      go(in, ByteVector.empty).stream

    /** Reassembles fragments, answers pings and stops after a close frame. */
    private def messages(in: Stream[F, Frame]): Stream[F, WsMessage] =
      def go(s: Stream[F, Frame], partial: Option[(Int, ByteVector)]): Pull[F, WsMessage, Unit] =
        s.pull.uncons1.flatMap {
          case None                => Pull.done
          case Some((frame, tail)) =>
            frame.opcode match
              case Frame.Ping => Pull.eval(write(Frame(true, Frame.Pong, frame.payload))) >> go(tail, partial)
              case Frame.Pong => go(tail, partial)
              case Frame.Close =>
                val code = Option.when(frame.payload.size >= 2)(frame.payload.take(2).toInt(signed = false))
                val reason = new String(frame.payload.drop(2).toArray, StandardCharsets.UTF_8)
                // Echo the close frame, as the protocol requires, then stop.
                Pull.output1(WsMessage.Close(code, reason)) >>
                  Pull.eval(
                    closeSent
                      .getAndSet(true)
                      .flatMap(sent => write(Frame(true, Frame.Close, frame.payload.take(2))).unlessA(sent))
                      .attempt
                      .void
                  )
              case Frame.Text | Frame.Binary if partial.isEmpty =>
                if frame.fin then Pull.output1(toMessage(frame.opcode, frame.payload)) >> go(tail, None)
                else go(tail, Some(frame.opcode -> frame.payload))
              case Frame.Continuation if partial.nonEmpty =>
                val (op, acc) = partial.get
                val data = acc ++ frame.payload
                if frame.fin then Pull.output1(toMessage(op, data)) >> go(tail, None)
                else go(tail, Some(op -> data))
              case other => Pull.raiseError[F](WebSocketException(s"Unexpected frame opcode $other"))
        }
      go(in, None).stream

    private def toMessage(opcode: Int, data: ByteVector): WsMessage =
      if opcode == Frame.Text then WsMessage.Text(new String(data.toArray, StandardCharsets.UTF_8))
      else WsMessage.Binary(data)

  private object Connection:
    def create[F[_]: Async](
        socket: Socket[F],
        leftover: ByteVector,
        random: SecureRandom[F]
    ): F[Connection[F]] =
      (Mutex[F], Ref.of[F, Boolean](false)).mapN(new Connection(socket, leftover, random, _, _))
