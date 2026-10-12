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

package discord.examples

import cats.effect.std.Console
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** A minimal stderr logger that works on every platform. */
final class ConsoleLogger[F[_]: Console: cats.Applicative](debugEnabled: Boolean = false) extends Logger[F]:
  private def log(level: String, msg: => String, t: Option[Throwable] = None): F[Unit] =
    Console[F].errorln(s"[$level] $msg${t.fold("")(e => s": $e")}")

  def error(message: => String): F[Unit] = log("error", message)
  def warn(message: => String): F[Unit] = log("warn", message)
  def info(message: => String): F[Unit] = log("info", message)
  def debug(message: => String): F[Unit] = if debugEnabled then log("debug", message) else ().pure[F]
  def trace(message: => String): F[Unit] = ().pure[F]
  def error(t: Throwable)(message: => String): F[Unit] = log("error", message, Some(t))
  def warn(t: Throwable)(message: => String): F[Unit] = log("warn", message, Some(t))
  def info(t: Throwable)(message: => String): F[Unit] = log("info", message, Some(t))
  def debug(t: Throwable)(message: => String): F[Unit] =
    if debugEnabled then log("debug", message, Some(t)) else ().pure[F]
  def trace(t: Throwable)(message: => String): F[Unit] = ().pure[F]
