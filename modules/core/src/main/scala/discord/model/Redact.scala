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

import io.circe.Json

/** Keeps secrets (tokens, keys) out of `toString` output and logs. */
object Redact:
  val Mask = "<redacted>"

  /** JSON keys whose values are secret. */
  val SecretKeys: Set[String] = Set("token", "secret_key")

  /** Renders `p` like a case class `toString`, with the named fields masked. */
  def product(p: Product, secretFields: String*): String =
    p.productElementNames
      .zip(p.productIterator)
      .map((name, value) => if secretFields.contains(name) then Mask else String.valueOf(value))
      .mkString(s"${p.productPrefix}(", ", ", ")")

  /** Masks the values of [[SecretKeys]] anywhere in `json`. */
  def json(json: Json): Json =
    json.arrayOrObject(
      json,
      arr => Json.fromValues(arr.map(this.json)),
      obj =>
        Json.fromFields(
          obj.toIterable.map((k, v) => k -> (if SecretKeys(k) then Json.fromString(Mask) else this.json(v)))
        )
    )
