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

package discord.json

import discord.model.Optional
import io.circe.{Encoder, Json}
import io.circe.syntax.*

/** Builds request bodies where absent fields are omitted rather than sent as `null`.
  *
  * {{{
  * import io.circe.syntax.*
  * import discord.json.JsonObj.*
  * JsonObj("content" := text, "embeds" :=? maybeEmbeds, "nick" :=! optionalNick)
  * }}}
  */
object JsonObj:
  type Field = (String, Json) | Option[(String, Json)]

  extension (key: String)
    /** Omitted when `None`. */
    def :=?[A: Encoder](a: Option[A]): Option[(String, Json)] = a.map(v => key -> v.asJson.deepDropNullValues)

    /** Omitted when `Missing`, `null` when `Null`. */
    def :=![A: Encoder](a: Optional[A]): Option[(String, Json)] =
      Optional.encode(a)(_.asJson.deepDropNullValues).map(key -> _)

  def apply(fields: Field*): Json =
    Json.fromFields(fields.flatMap {
      case o: Option[?]                  => o.asInstanceOf[Option[(String, Json)]]
      case kv: (String, Json) @unchecked => Some(kv)
    })
