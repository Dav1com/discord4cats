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

import cats.syntax.all.*
import discord.model.*

/** Builds commands whose handlers get typed option values.
  *
  * {{{
  * val cmds = Commands[IO]
  * val greet = cmds.slash("greet", "Greets someone", Opt.user("who", "Who to greet")) { (ctx, who) =>
  *   ctx.reply(s"Hello ${who.mention}!")
  * }
  * }}}
  */
final class Commands[F[_]]:
  private type Ctx = CommandContext[F]
  private type Raw = (List[InteractionDataOption], ResolvedData)

  private def get[A](o: Opt[A], raw: Raw): Either[String, A] = o.read(raw._1.find(_.name == o.name), raw._2)

  private def make(name: String, description: String, opts: List[Opt[?]])(
      run: (Ctx, Raw) => Either[String, F[Unit]]
  ): SlashCommand[F] =
    SlashCommand[F](name, description, opts.map(_.spec), (ctx, o, r) => run(ctx, (o, r)))

  def slash(name: String, description: String)(handler: Ctx => F[Unit]): SlashCommand[F] =
    make(name, description, Nil)((ctx, _) => Right(handler(ctx)))

  def slash[A](name: String, description: String, a: Opt[A])(
      handler: (Ctx, A) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a))((ctx, raw) => get(a, raw).map(handler(ctx, _)))

  def slash[A, B](name: String, description: String, a: Opt[A], b: Opt[B])(
      handler: (Ctx, A, B) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a, b))((ctx, raw) =>
      (get(a, raw), get(b, raw)).mapN((vA, vB) => handler(ctx, vA, vB))
    )

  def slash[A, B, C](name: String, description: String, a: Opt[A], b: Opt[B], c: Opt[C])(
      handler: (Ctx, A, B, C) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a, b, c))((ctx, raw) =>
      (get(a, raw), get(b, raw), get(c, raw)).mapN((vA, vB, vC) => handler(ctx, vA, vB, vC))
    )

  def slash[A, B, C, D](name: String, description: String, a: Opt[A], b: Opt[B], c: Opt[C], d: Opt[D])(
      handler: (Ctx, A, B, C, D) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a, b, c, d))((ctx, raw) =>
      (get(a, raw), get(b, raw), get(c, raw), get(d, raw)).mapN((vA, vB, vC, vD) =>
        handler(ctx, vA, vB, vC, vD)
      )
    )

  def slash[A, B, C, D, E](
      name: String,
      description: String,
      a: Opt[A],
      b: Opt[B],
      c: Opt[C],
      d: Opt[D],
      e: Opt[E]
  )(
      handler: (Ctx, A, B, C, D, E) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a, b, c, d, e))((ctx, raw) =>
      (get(a, raw), get(b, raw), get(c, raw), get(d, raw), get(e, raw)).mapN((vA, vB, vC, vD, vE) =>
        handler(ctx, vA, vB, vC, vD, vE)
      )
    )

  def slash[A, B, C, D, E, G](
      name: String,
      description: String,
      a: Opt[A],
      b: Opt[B],
      c: Opt[C],
      d: Opt[D],
      e: Opt[E],
      g: Opt[G]
  )(
      handler: (Ctx, A, B, C, D, E, G) => F[Unit]
  ): SlashCommand[F] =
    make(name, description, List(a, b, c, d, e, g))((ctx, raw) =>
      (get(a, raw), get(b, raw), get(c, raw), get(d, raw), get(e, raw), get(g, raw)).mapN(
        (vA, vB, vC, vD, vE, vG) => handler(ctx, vA, vB, vC, vD, vE, vG)
      )
    )

  /** A command made of subcommands (built with [[slash]]) and subcommand groups (built with [[subgroup]]). */
  def group(name: String, description: String)(
      children: (SlashCommand[F] | SubcommandGroup[F])*
  ): GroupCommand[F] =
    GroupCommand(name, description, children.toList)

  def subgroup(name: String, description: String)(subcommands: SlashCommand[F]*): SubcommandGroup[F] =
    SubcommandGroup(name, description, subcommands.toList)

  /** A user context menu command. */
  def user(name: String)(handler: (Ctx, User) => F[Unit]): UserCommand[F] = UserCommand(name, handler)

  /** A message context menu command. */
  def message(name: String)(handler: (Ctx, Message) => F[Unit]): MessageCommand[F] =
    MessageCommand(name, handler)

object Commands:
  def apply[F[_]]: Commands[F] = new Commands[F]
