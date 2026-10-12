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

import cats.{Order, Show}
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

/** A Discord ID. Encoded as a JSON string, as Discord does, but decoded from either a string or a number. */
opaque type Snowflake = Long

object Snowflake:
  /** First millisecond of 2015, the epoch Discord snowflakes count from. */
  val DiscordEpoch: Long = 1420070400000L

  def apply(value: Long): Snowflake = value

  def parse(s: String): Option[Snowflake] = s.toLongOption

  /** Lowest snowflake that would be generated at the given unix time; useful for pagination. */
  def fromTimestampMillis(millis: Long): Snowflake = (millis - DiscordEpoch) << 22

  extension (s: Snowflake)
    def value: Long = s
    def asString: String = s.toString
    def timestampMillis: Long = (s >>> 22) + DiscordEpoch

  given Encoder[Snowflake] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Snowflake] =
    Decoder.decodeString.emap(s => parse(s).toRight(s"Invalid snowflake: $s")).or(Decoder.decodeLong)
  given KeyEncoder[Snowflake] = KeyEncoder.encodeKeyString.contramap(_.toString)
  given KeyDecoder[Snowflake] = KeyDecoder.instance(parse)
  given Order[Snowflake] = Order.fromLessThan(_ < _)
  given Ordering[Snowflake] = Ordering.Long
  given Show[Snowflake] = Show.fromToString
