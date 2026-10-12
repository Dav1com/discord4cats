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

import cats.effect.{IO, Resource}
import cats.effect.std.Queue
import discord.gateway.ws.{WsClient, WsConnection, WsMessage}
import fs2.Stream
import io.circe.Json
import io.circe.parser.parse
import org.http4s.Uri

/** One fake WebSocket connection, seen from the server side. */
final class FakeConn(val uri: Uri, toClient: Queue[IO, Option[WsMessage]], fromClient: Queue[IO, WsMessage])
    extends WsConnection[IO]:
  def receive: Stream[IO, WsMessage] = Stream.fromQueueNoneTerminated(toClient)
  def send(message: WsMessage): IO[Unit] = fromClient.offer(message)

  def push(json: Json): IO[Unit] = toClient.offer(Some(WsMessage.Text(json.noSpaces)))
  def pushBinary(data: scodec.bits.ByteVector): IO[Unit] = toClient.offer(Some(WsMessage.Binary(data)))
  def serverClose(code: Int): IO[Unit] =
    toClient.offer(Some(WsMessage.Close(Some(code), "test"))) >> toClient.offer(None)

  /** The next message the client sent. */
  def next: IO[WsMessage] = fromClient.take

  /** The next JSON text message the client sent, skipping anything else. */
  def nextJson: IO[Json] = next.flatMap {
    case WsMessage.Text(t) => IO.fromEither(parse(t))
    case _                 => nextJson
  }

  /** The next JSON message with this opcode. */
  def nextOp(op: Int): IO[Json] =
    nextJson.flatMap(j => if j.hcursor.get[Int]("op").contains(op) then IO.pure(j) else nextOp(op))

/** A WsClient whose connections the test drives. */
final class FakeWs(connections: Queue[IO, FakeConn]) extends WsClient[IO]:
  def connect(uri: Uri): Resource[IO, WsConnection[IO]] =
    Resource.eval(for
      in <- Queue.unbounded[IO, Option[WsMessage]]
      out <- Queue.unbounded[IO, WsMessage]
      conn = FakeConn(uri, in, out)
      _ <- connections.offer(conn)
    yield conn)

  /** Waits for the client to open its next connection. */
  def accept: IO[FakeConn] = connections.take

object FakeWs:
  def apply(): IO[FakeWs] = Queue.unbounded[IO, FakeConn].map(new FakeWs(_))
