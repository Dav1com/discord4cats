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

package discord.model

import io.circe.{ACursor, Decoder, HCursor, Json}

/** Discord's tri-state fields: a key can be absent, explicitly `null`, or hold a value.
  *
  * In PATCH payloads, `Missing` leaves a field untouched while `Null` clears it.
  */
enum Optional[+A]:
  case Missing
  case Null
  case Present(value: A)

  def toOption: Option[A] = this match
    case Present(a) => Some(a)
    case _          => None

  def map[B](f: A => B): Optional[B] = this match
    case Present(a) => Present(f(a))
    case Missing    => Missing
    case Null       => Null

  def isMissing: Boolean = this == Missing

object Optional:
  def apply[A](a: A): Optional[A] = Present(a)

  /** `None` becomes `Null`. */
  def fromOption[A](o: Option[A]): Optional[A] = o.fold(Null)(Present(_))

  given [A](using d: Decoder[A]): Decoder[Optional[A]] = new Decoder[Optional[A]]:
    def apply(c: HCursor): Decoder.Result[Optional[A]] =
      if c.value.isNull then Right(Null) else d(c).map(Present(_))

    // A missing key arrives as a failed cursor, which circe never passes to `apply`.
    override def tryDecode(c: ACursor): Decoder.Result[Optional[A]] = c match
      case hc: HCursor => apply(hc)
      case _           => Right(Missing)

  private[discord] def encode[A](o: Optional[A])(f: A => Json): Option[Json] = o match
    case Missing    => None
    case Null       => Some(Json.Null)
    case Present(a) => Some(f(a))
