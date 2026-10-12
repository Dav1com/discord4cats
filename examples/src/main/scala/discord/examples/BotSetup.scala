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

import cats.effect.{IO, Resource}
import cats.effect.std.Env
import discord.gateway.{Gateway, GatewayConfig}
import discord.model.{Intents, Snowflake}
import discord.rest.DiscordRest
import org.http4s.ember.client.EmberClientBuilder

/** What every example needs: a REST client and a gateway connection. */
final case class Bot(rest: DiscordRest[IO], gateway: Gateway[IO], devGuild: Option[Snowflake])

object Bot:
  val logger = ConsoleLogger[IO]()

  /** Reads `DISCORD_TOKEN`, and optionally `DISCORD_GUILD_ID` to register commands in one guild (instant)
    * instead of globally.
    */
  def resource(intents: Intents): Resource[IO, Bot] =
    for
      token <- Resource.eval(
        Env[IO]
          .get("DISCORD_TOKEN")
          .flatMap(IO.fromOption(_)(new IllegalArgumentException("Set DISCORD_TOKEN")))
      )
      guild <- Resource.eval(Env[IO].get("DISCORD_GUILD_ID").map(_.flatMap(Snowflake.parse)))
      http <- EmberClientBuilder.default[IO].build
      rest <- Resource.eval(DiscordRest[IO](http, token))
      gateway <- Gateway[IO](rest, GatewayConfig(token, intents), Some(logger))
    yield Bot(rest, gateway, guild)
