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

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Network, Socket}
import munit.CatsEffectSuite
import org.http4s.Uri
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets

/** Runs the real client against a minimal WebSocket server on a loopback socket. */
class WsClientSuite extends CatsEffectSuite:

  private def readHead(socket: Socket[IO], acc: ByteVector = ByteVector.empty): IO[String] =
    val end = acc.indexOfSlice(ByteVector('\r', '\n', '\r', '\n'))
    if end >= 0 then IO.pure(new String(acc.take(end).toArray, StandardCharsets.ISO_8859_1))
    else socket.read(4096).flatMap(c => readHead(socket, acc ++ c.get.toByteVector))

  private def readFrame(socket: Socket[IO], buf: ByteVector = ByteVector.empty): IO[(Frame, ByteVector)] =
    Frame.decode(buf) match
      case Right(Some(r)) => IO.pure(r)
      case Right(None)    => socket.read(4096).flatMap(c => readFrame(socket, buf ++ c.get.toByteVector))
      case Left(e)        => IO.raiseError(new Exception(e))

  private def text(s: String) = ByteVector.view(s.getBytes(StandardCharsets.UTF_8))

  /** Handshakes, sends a fragmented text message and a ping, then reports what the client sent back. */
  private def serve(socket: Socket[IO]): IO[List[Frame]] =
    for
      head <- readHead(socket)
      key = head
        .split("\r\n")
        .collectFirst {
          case l if l.toLowerCase.startsWith("sec-websocket-key:") => l.drop(18).trim
        }
        .get
      accept = Sha1(text(key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")).toBase64
      _ <- socket.write(
        Chunk.array(
          s"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: $accept\r\n\r\n"
            .getBytes(StandardCharsets.US_ASCII)
        )
      )
      out = List(
        Frame(fin = false, Frame.Text, text("hel")),
        Frame(fin = true, Frame.Ping, text("p")),
        Frame(fin = true, Frame.Continuation, text("lo")),
        Frame(fin = true, Frame.Binary, ByteVector(1, 2, 3))
      ).foldLeft(ByteVector.empty)((acc, f) => acc ++ Frame.encode(f, None))
      _ <- socket.write(Chunk.byteVector(out))
      f1 <- readFrame(socket)
      f2 <- readFrame(socket, f1._2)
      _ <- socket.write(
        Chunk.byteVector(Frame.encode(Frame(true, Frame.Close, ByteVector.fromInt(1000, 2)), None))
      )
      f3 <- readFrame(socket, f2._2)
    yield List(f1._1, f2._1, f3._1)

  test("handshake, fragmented messages, ping/pong and close against a real socket") {
    Network[IO].bind(SocketAddress(ip"127.0.0.1", Port.Wildcard)).use { server =>
      for
        addr <- IO.pure(server.address)
        serverSide <- server.accept.head.evalMap(serve).compile.lastOrError.start
        uri = Uri.unsafeFromString(s"ws://127.0.0.1:${addr.asIpUnsafe.port.value}/gateway?v=10")
        received <- WsClient[IO].connect(uri).use { conn =>
          conn.receive
            .evalTap {
              case WsMessage.Text(t) => conn.sendText(t.toUpperCase)
              case _                 => IO.unit
            }
            .compile
            .toList
        }
        clientFrames <- serverSide.joinWithNever
      yield
        assertEquals(
          received,
          List(
            WsMessage.Text("hello"),
            WsMessage.Binary(ByteVector(1, 2, 3)),
            WsMessage.Close(Some(1000), "")
          )
        )
        assertEquals(
          clientFrames.map(f => (f.opcode, f.payload)),
          List(
            (Frame.Pong, text("p")),
            (Frame.Text, text("HELLO")),
            (Frame.Close, ByteVector.fromInt(1000, 2))
          )
        )
    }
  }
