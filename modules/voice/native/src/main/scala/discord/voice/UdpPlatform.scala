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

import cats.effect.{Async, FileDescriptorPoller, IO, LiftIO, Resource}
import fs2.io.net.Network
import scodec.bits.ByteVector

import scala.scalanative.posix.arpa.inet.{htons, inet_pton}
import scala.scalanative.posix.errno.*
import scala.scalanative.posix.fcntl.*
import scala.scalanative.posix.netinet.in.*
import scala.scalanative.posix.netinet.inOps.*
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{AF_INET, SOCK_DGRAM, sockaddr}
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** fs2 has no UDP on Scala Native yet, so this is a small non-blocking POSIX datagram socket, driven by the
  * cats-effect file descriptor poller just like fs2's own TCP sockets.
  */
private[voice] object UdpPlatform:
  private val MaxDatagram = 2048

  def connect[F[_]: Async: Network: LiftIO](ip: String, port: Int): Resource[F, UdpSocket[F]] =
    val acquire = IO {
      val fd = socket.socket(AF_INET, SOCK_DGRAM, 0)
      if fd < 0 then throw new java.io.IOException(s"socket() failed: errno $errno")
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK)
      Zone {
        val addr = alloc[sockaddr_in]()
        addr.sin_family = AF_INET.toUShort
        addr.sin_port = htons(port.toUShort)
        if inet_pton(AF_INET, toCString(ip), addr.sin_addr.at1.asInstanceOf[Ptr[Byte]]) != 1 then
          unistd.close(fd)
          throw new IllegalArgumentException(s"Invalid IPv4 address: $ip")
        // Connecting a UDP socket only sets the default destination.
        if socket.connect(fd, addr.asInstanceOf[Ptr[sockaddr]], sizeof[sockaddr_in].toUInt) != 0 then
          val e = errno
          unistd.close(fd)
          throw new java.io.IOException(s"connect() failed: errno $e")
      }
      fd
    }
    val resource: Resource[IO, UdpSocket[F]] =
      for
        poller <- Resource.eval(
          IO.pollers.flatMap(ps =>
            IO.fromOption(ps.collectFirst { case p: FileDescriptorPoller => p })(
              new IllegalStateException("No FileDescriptorPoller installed")
            )
          )
        )
        fd <- Resource.make(acquire)(fd => IO(unistd.close(fd)).void)
        handle <- poller.registerFileDescriptor(fd, true, true)
      yield new UdpSocket[F]:
        def send(packet: ByteVector): F[Unit] =
          val bytes = packet.toArray
          handle
            .pollWriteRec(()) { _ =>
              IO {
                val n = socket.send(fd, bytes.atUnsafe(0), bytes.length.toUSize, 0)
                if n >= 0 then Right(())
                else if errno == EAGAIN || errno == EWOULDBLOCK then Left(())
                else throw new java.io.IOException(s"send() failed: errno $errno")
              }
            }
            .to[F]

        def receive: F[ByteVector] =
          val buf = new Array[Byte](MaxDatagram)
          handle
            .pollReadRec(()) { _ =>
              IO {
                val n = socket.recv(fd, buf.atUnsafe(0), buf.length.toUSize, 0)
                if n >= 0 then Right(ByteVector(buf, 0, n.toInt))
                else if errno == EAGAIN || errno == EWOULDBLOCK then Left(())
                else throw new java.io.IOException(s"recv() failed: errno $errno")
              }
            }
            .to[F]
    resource.mapK(LiftIO.liftK[F])
