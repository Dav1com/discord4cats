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

package discord.commands

import discord.model.*
import io.circe.{Decoder, Json}

/** A typed slash command option: what gets registered with Discord, and how to read the value back. */
final case class Opt[A](
    spec: ApplicationCommandOption,
    read: (Option[InteractionDataOption], ResolvedData) => Either[String, A]
):
  def name: String = spec.name

  /** Makes the option optional. */
  def optional: Opt[Option[A]] =
    Opt(
      spec.copy(required = Some(false)),
      (o, r) => o.filter(_.value.exists(!_.isNull)).fold(Right(None))(v => read(Some(v), r).map(Some(_)))
    )

  def withDefault(default: A): Opt[A] = optional.map(_.getOrElse(default))

  def map[B](f: A => B): Opt[B] = Opt(spec, (o, r) => read(o, r).map(f))

  /** Validates the value; `Left` is shown to the user as an error. */
  def emap[B](f: A => Either[String, B]): Opt[B] = Opt(spec, (o, r) => read(o, r).flatMap(f))

object Opt:
  import ApplicationCommandOptionType as T

  private def spec(t: ApplicationCommandOptionType, name: String, description: String) =
    ApplicationCommandOption(t, name, description, required = Some(true))

  private def value[A: Decoder](
      name: String
  ): (Option[InteractionDataOption], ResolvedData) => Either[String, A] =
    (o, _) =>
      o.flatMap(_.value) match
        case None    => Left(s"Missing option `$name`")
        case Some(j) => j.as[A].left.map(_ => s"Invalid value for `$name`")

  private def resolved[A](name: String)(
      find: (Snowflake, ResolvedData) => Option[A]
  ): (Option[InteractionDataOption], ResolvedData) => Either[String, A] =
    (o, r) =>
      value[Snowflake](name).apply(o, r).flatMap(id => find(id, r).toRight(s"Could not resolve `$name`"))

  def string(
      name: String,
      description: String,
      choices: List[(String, String)] = Nil,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None
  ): Opt[String] =
    Opt(
      spec(T.String, name, description).copy(
        choices = Option.when(choices.nonEmpty)(choices.map(ApplicationCommandOptionChoice.string)),
        minLength = minLength,
        maxLength = maxLength
      ),
      value[String](name)
    )

  def int(
      name: String,
      description: String,
      min: Option[Long] = None,
      max: Option[Long] = None,
      choices: List[(String, Long)] = Nil
  ): Opt[Long] =
    Opt(
      spec(T.Integer, name, description).copy(
        minValue = min.map(Json.fromLong),
        maxValue = max.map(Json.fromLong),
        choices = Option.when(choices.nonEmpty)(choices.map(ApplicationCommandOptionChoice.int))
      ),
      value[Long](name)
    )

  def number(
      name: String,
      description: String,
      min: Option[Double] = None,
      max: Option[Double] = None
  ): Opt[Double] =
    Opt(
      spec(T.Number, name, description).copy(
        minValue = min.map(Json.fromDoubleOrNull),
        maxValue = max.map(Json.fromDoubleOrNull)
      ),
      value[Double](name)
    )

  def bool(name: String, description: String): Opt[Boolean] =
    Opt(spec(T.Boolean, name, description), value[Boolean](name))

  def user(name: String, description: String): Opt[User] =
    Opt(spec(T.User, name, description), resolved(name)((id, r) => r.users.get(id)))

  /** The member behind a user option, with the user filled in. Only resolves in guilds. */
  def member(name: String, description: String): Opt[GuildMember] =
    Opt(
      spec(T.User, name, description),
      resolved(name)((id, r) => r.members.get(id).map(m => m.copy(user = m.user.orElse(r.users.get(id)))))
    )

  def channel(name: String, description: String, types: List[ChannelType] = Nil): Opt[Channel] =
    Opt(
      spec(T.Channel, name, description).copy(channelTypes = Option.when(types.nonEmpty)(types)),
      resolved(name)((id, r) => r.channels.get(id))
    )

  def role(name: String, description: String): Opt[Role] =
    Opt(spec(T.Role, name, description), resolved(name)((id, r) => r.roles.get(id)))

  /** A user or a role: `Left(user)` or `Right(role)`. */
  def mentionable(name: String, description: String): Opt[Either[User, Role]] =
    Opt(
      spec(T.Mentionable, name, description),
      resolved(name)((id, r) => r.users.get(id).map(Left(_)).orElse(r.roles.get(id).map(Right(_))))
    )

  def attachment(name: String, description: String): Opt[Attachment] =
    Opt(spec(T.Attachment, name, description), resolved(name)((id, r) => r.attachments.get(id)))
