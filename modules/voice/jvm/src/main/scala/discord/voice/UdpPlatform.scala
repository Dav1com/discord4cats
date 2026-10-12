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

import cats.effect.{Async, LiftIO, Resource}
import com.comcast.ip4s.{IpAddress, Ipv4Address, Port, SocketAddress}
import fs2.Chunk
import fs2.io.net.Network
import scodec.bits.ByteVector

private[voice] object UdpPlatform:
  def connect[F[_]: Async: Network: LiftIO](ip: String, port: Int): Resource[F, UdpSocket[F]] =
    for
      remote <- Resource.eval(
        Async[F].fromOption(
          (IpAddress.fromString(ip), Port.fromInt(port)).mapN(SocketAddress(_, _)),
          IllegalArgumentException(s"Invalid UDP address $ip:$port")
        )
      )
      local = remote.host match
        case _: Ipv4Address => SocketAddress(Ipv4Address.Wildcard, Port.Wildcard)
        case _              => SocketAddress.Wildcard
      socket <- Network[F].bindDatagramSocket(local)
    yield new UdpSocket[F]:
      def send(packet: ByteVector): F[Unit] = socket.write(Chunk.byteVector(packet), remote)
      def receive: F[ByteVector] = Async[F].map(socket.read)(_.bytes.toByteVector)

  extension [A, B](t: (Option[A], Option[B]))
    private def mapN[C](f: (A, B) => C): Option[C] =
      for a <- t._1; b <- t._2 yield f(a, b)
