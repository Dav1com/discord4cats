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

import fs2.{Pipe, Pull, RaiseThrowable, Stream}
import scodec.bits.ByteVector

/** Reads Opus packets out of an Ogg Opus file (`.opus`), in pure Scala, on every platform. */
object OggOpus:

  final class OggException(message: String) extends RuntimeException(message)

  private val CapturePattern = ByteVector('O', 'g', 'g', 'S')
  private val OpusHead = ByteVector('O', 'p', 'u', 's', 'H', 'e', 'a', 'd')
  private val OpusTags = ByteVector('O', 'p', 'u', 's', 'T', 'a', 'g', 's')

  /** An Ogg page: its segment table already split into packets. `continued` packets started on a previous
    * page; `lastIncomplete` is true when the final packet continues on the next page.
    */
  private final case class Page(continued: Boolean, packets: Vector[ByteVector], lastIncomplete: Boolean)

  /** Parses one page from the front of `buf`. */
  private def parsePage(buf: ByteVector): Either[String, Option[(Page, ByteVector)]] =
    if buf.size < 27 then Right(None)
    else if buf.take(4) != CapturePattern then Left("Not an Ogg stream (missing OggS capture pattern)")
    else
      val headerType = buf(5) & 0xff
      val segments = buf(26) & 0xff
      if buf.size < 27 + segments then Right(None)
      else
        val lacing = buf.slice(27, 27 + segments).toArray.map(_ & 0xff)
        val bodySize = lacing.sum.toLong
        val start = 27L + segments
        if buf.size < start + bodySize then Right(None)
        else
          val body = buf.slice(start, start + bodySize)
          // Packets end at the first lacing value < 255.
          val (packets, last, _) = lacing.foldLeft((Vector.empty[ByteVector], ByteVector.empty, 0L)) {
            case ((done, current, offset), len) =>
              val next = current ++ body.slice(offset, offset + len)
              if len < 255 then (done :+ next, ByteVector.empty, offset + len) else (done, next, offset + len)
          }
          val incomplete = lacing.lastOption.contains(255)
          val all = if incomplete then packets :+ last else packets
          Right(Some(Page((headerType & 0x01) != 0, all, incomplete) -> buf.drop(start + bodySize)))

  /** Raw Ogg packets, joined across pages. */
  def packets[F[_]: RaiseThrowable]: Pipe[F, Byte, ByteVector] = in =>
    def go(s: Stream[F, Byte], buf: ByteVector, carry: Option[ByteVector]): Pull[F, ByteVector, Unit] =
      parsePage(buf) match
        case Left(err)                 => Pull.raiseError[F](OggException(err))
        case Right(Some((page, rest))) =>
          val joined = (carry, page.packets.headOption) match
            case (Some(c), Some(first)) if page.continued => (c ++ first) +: page.packets.tail
            case _                                        => page.packets
          val (complete, nextCarry) =
            if page.lastIncomplete && joined.nonEmpty then (joined.init, Some(joined.last))
            else (joined, None)
          Pull.output(fs2.Chunk.from(complete)) >> go(s, rest, nextCarry)
        case Right(None) =>
          s.pull.uncons.flatMap {
            case Some((chunk, tail)) => go(tail, buf ++ chunk.toByteVector, carry)
            case None                =>
              if buf.isEmpty then Pull.done else Pull.raiseError[F](OggException("Truncated Ogg page"))
          }
    go(in, ByteVector.empty, None).stream

  /** Opus audio frames from an Ogg Opus file. Fails unless the stream starts with an `OpusHead` header. */
  def frames[F[_]: RaiseThrowable]: Pipe[F, Byte, OpusFrame] = in =>
    in.through(packets)
      .pull
      .uncons1
      .flatMap {
        case Some((head, rest)) if head.startsWith(OpusHead) =>
          rest.filterNot(_.startsWith(OpusTags)).filter(_.nonEmpty).map(OpusFrame(_)).pull.echo
        case Some(_) => Pull.raiseError[F](OggException("Not an Ogg Opus stream (missing OpusHead)"))
        case None    => Pull.done
      }
      .stream
