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

package discord.rest

import org.http4s.{Method, Uri}

/** A REST endpoint: the HTTP method, the path template and the concrete URI.
  *
  * The rate limit key combines method, template and the "major parameter" (the channel, guild or webhook ID),
  * since Discord rate limits each major parameter independently.
  */
final case class Route(method: Method, template: String, uri: Uri, major: String):
  def key: String = s"${method.name} $template $major"
  override def toString: String = s"${method.name} $template"

object Route:
  private val MajorRoots = Set("channels", "guilds", "webhooks", "interactions")

  /** Builds a route from a template like `/channels/{channel.id}/messages`. Each `{...}` placeholder is
    * replaced in order by an argument, which gets percent-encoded as a path segment.
    */
  def apply(base: Uri, method: Method, template: String, args: String*): Route =
    val segments = template.split('/').filter(_.nonEmpty).toList
    val (uri, _) = segments.foldLeft((base, args.toList)) {
      case ((u, a :: rest), s) if s.startsWith("{") => (u / a, rest)
      case ((u, rest), s)                           => (u / s, rest)
    }
    val major = if segments.headOption.exists(MajorRoots) then args.headOption.getOrElse("") else ""
    Route(method, template, uri, major)
