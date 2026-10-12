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

import cats.effect.Async
import cats.syntax.all.*
import discord.model.*
import fs2.Chunk
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import org.http4s.{Message as _, *}
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, `Content-Type`, `User-Agent`}
import org.http4s.multipart.{Multiparts, Part}
import org.http4s.syntax.literals.*
import org.typelevel.ci.*

/** Discord's HTTP API.
  *
  * Rate limits are handled transparently (see [[RateLimiter]]). Failed calls raise [[DiscordError]].
  */
trait DiscordRest[F[_]]:
  // users & application
  def getCurrentUser: F[User]
  def getUser(userId: Snowflake): F[User]
  def getCurrentApplication: F[Application]
  def getGatewayBot: F[GatewayBot]
  def createDM(recipientId: Snowflake): F[Channel]

  // channels
  def getChannel(channelId: Snowflake): F[Channel]
  def modifyChannel(channelId: Snowflake, modify: ModifyChannel, reason: Option[String] = None): F[Channel]
  def deleteChannel(channelId: Snowflake, reason: Option[String] = None): F[Channel]
  def triggerTyping(channelId: Snowflake): F[Unit]

  // messages
  def getMessage(channelId: Snowflake, messageId: Snowflake): F[Message]
  def getMessages(
      channelId: Snowflake,
      limit: Int = 50,
      before: Option[Snowflake] = None,
      after: Option[Snowflake] = None
  ): F[List[Message]]
  def createMessage(channelId: Snowflake, message: CreateMessage): F[Message]
  def editMessage(channelId: Snowflake, messageId: Snowflake, edit: EditMessage): F[Message]
  def deleteMessage(channelId: Snowflake, messageId: Snowflake, reason: Option[String] = None): F[Unit]

  /** @param emoji a unicode emoji, or `name:id` for a custom one */
  def createReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): F[Unit]
  def deleteOwnReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): F[Unit]

  // guilds
  def getGuild(guildId: Snowflake): F[Guild]
  def getGuildChannels(guildId: Snowflake): F[List[Channel]]
  def getGuildMember(guildId: Snowflake, userId: Snowflake): F[GuildMember]
  def listGuildMembers(
      guildId: Snowflake,
      limit: Int = 100,
      after: Option[Snowflake] = None
  ): F[List[GuildMember]]
  def getGuildRoles(guildId: Snowflake): F[List[Role]]
  def addGuildMemberRole(
      guildId: Snowflake,
      userId: Snowflake,
      roleId: Snowflake,
      reason: Option[String] = None
  ): F[Unit]
  def removeGuildMemberRole(
      guildId: Snowflake,
      userId: Snowflake,
      roleId: Snowflake,
      reason: Option[String] = None
  ): F[Unit]

  // interactions
  def createInteractionResponse(
      interactionId: Snowflake,
      token: String,
      response: InteractionResponse
  ): F[Unit]
  def getOriginalInteractionResponse(applicationId: Snowflake, token: String): F[Message]
  def editOriginalInteractionResponse(applicationId: Snowflake, token: String, edit: EditMessage): F[Message]
  def deleteOriginalInteractionResponse(applicationId: Snowflake, token: String): F[Unit]
  def createFollowupMessage(applicationId: Snowflake, token: String, message: CreateMessage): F[Message]

  // application commands
  def getGlobalCommands(applicationId: Snowflake): F[List[ApplicationCommand]]
  def bulkOverwriteGlobalCommands(
      applicationId: Snowflake,
      commands: List[ApplicationCommand]
  ): F[List[ApplicationCommand]]
  def getGuildCommands(applicationId: Snowflake, guildId: Snowflake): F[List[ApplicationCommand]]
  def bulkOverwriteGuildCommands(
      applicationId: Snowflake,
      guildId: Snowflake,
      commands: List[ApplicationCommand]
  ): F[List[ApplicationCommand]]

object DiscordRest:
  val DefaultBaseUri: Uri = uri"https://discord.com/api/v10"
  val LibraryUrl = "https://github.com/davicom/discord-scala"
  val LibraryVersion = "0.1"

  /** @param token the bot token, without the `Bot ` prefix */
  def apply[F[_]: Async](client: Client[F], token: String, baseUri: Uri = DefaultBaseUri): F[DiscordRest[F]] =
    RateLimiter[F]().map(new Impl(client, token, baseUri, _))

  private final class Impl[F[_]](client: Client[F], token: String, base: Uri, limiter: RateLimiter[F])(using
      F: Async[F]
  ) extends DiscordRest[F]:
    import Method.*

    private val defaultHeaders = Headers(
      Authorization(Credentials.Token(ci"Bot", token)),
      `User-Agent`(ProductId("DiscordBot", Some(s"($LibraryUrl, $LibraryVersion)")))
    )

    private def route(method: Method, template: String, args: Any*): Route =
      Route(base, method, template, args.map(_.toString)*)

    private def reasonHeader(reason: Option[String]): Headers =
      Headers(reason.map(r => Header.Raw(ci"X-Audit-Log-Reason", Uri.encode(r))).toList)

    private def send[A](r: Route, req: Request[F])(handle: Response[F] => F[A]): F[A] =
      limiter.run(r)(client.run(req.putHeaders(defaultHeaders))) { resp =>
        if resp.status.isSuccess then handle(resp)
        else
          resp.attemptAs[Json].value.flatMap { body =>
            val parsed = body.toOption.flatMap(_.as[DiscordError.Body].toOption)
            F.raiseError(
              DiscordError.Api(
                r.toString,
                resp.status.code,
                parsed.flatMap(_.code),
                parsed.flatMap(_.message).getOrElse(resp.status.reason),
                parsed.flatMap(_.errors)
              )
            )
          }
      }

    private def decodeAs[A: Decoder](r: Route)(resp: Response[F]): F[A] =
      resp.as[Json].flatMap(j => F.fromEither(j.as[A].leftMap(DiscordError.Decoding(r.toString, _))))

    private def call[A: Decoder](r: Route, headers: Headers = Headers.empty): F[A] =
      send(r, Request[F](r.method, r.uri, headers = headers))(decodeAs[A](r))

    private def callWith[B: Encoder, A: Decoder](r: Route, body: B, headers: Headers = Headers.empty): F[A] =
      send(r, Request[F](r.method, r.uri, headers = headers).withEntity(body.asJson))(decodeAs[A](r))

    private def exec(r: Route, headers: Headers = Headers.empty): F[Unit] =
      send(r, Request[F](r.method, r.uri, headers = headers))(_.body.compile.drain)

    private def execWith[B: Encoder](r: Route, body: B): F[Unit] =
      send(r, Request[F](r.method, r.uri).withEntity(body.asJson))(_.body.compile.drain)

    /** JSON body, or multipart (`payload_json` + `files[n]`) when there are files. */
    private def callWithFiles[B: Encoder, A: Decoder](r: Route, body: B, files: List[FileUpload]): F[A] =
      if files.isEmpty then callWith[B, A](r, body)
      else
        Multiparts.forSync[F].flatMap { mp =>
          val payload =
            Part.formData[F]("payload_json", body.asJson.noSpaces, `Content-Type`(MediaType.application.json))
          val parts = files.zipWithIndex.map { (f, i) =>
            val ct = MediaType.parse(f.contentType).getOrElse(MediaType.application.`octet-stream`)
            Part.fileData[F](
              s"files[$i]",
              f.filename,
              fs2.Stream.chunk(Chunk.byteVector(f.content)),
              `Content-Type`(ct)
            )
          }
          mp.multipart(Vector(payload) ++ parts).flatMap { m =>
            send(r, Request[F](r.method, r.uri).withEntity(m).putHeaders(m.headers))(decodeAs[A](r))
          }
        }

    def getCurrentUser: F[User] = call(route(GET, "/users/@me"))
    def getUser(userId: Snowflake): F[User] = call(route(GET, "/users/{user.id}", userId))
    def getCurrentApplication: F[Application] = call(route(GET, "/applications/@me"))
    def getGatewayBot: F[GatewayBot] = call(route(GET, "/gateway/bot"))
    def createDM(recipientId: Snowflake): F[Channel] =
      callWith(route(POST, "/users/@me/channels"), Json.obj("recipient_id" := recipientId))

    def getChannel(channelId: Snowflake): F[Channel] = call(route(GET, "/channels/{channel.id}", channelId))
    def modifyChannel(channelId: Snowflake, modify: ModifyChannel, reason: Option[String]): F[Channel] =
      callWith(route(PATCH, "/channels/{channel.id}", channelId), modify, reasonHeader(reason))
    def deleteChannel(channelId: Snowflake, reason: Option[String]): F[Channel] =
      call(route(DELETE, "/channels/{channel.id}", channelId), reasonHeader(reason))
    def triggerTyping(channelId: Snowflake): F[Unit] = exec(
      route(POST, "/channels/{channel.id}/typing", channelId)
    )

    def getMessage(channelId: Snowflake, messageId: Snowflake): F[Message] =
      call(route(GET, "/channels/{channel.id}/messages/{message.id}", channelId, messageId))
    def getMessages(
        channelId: Snowflake,
        limit: Int,
        before: Option[Snowflake],
        after: Option[Snowflake]
    ): F[List[Message]] =
      val r = route(GET, "/channels/{channel.id}/messages", channelId)
      val uri = r.uri
        .withQueryParam("limit", limit)
        .withOptionQueryParam("before", before.map(_.asString))
        .withOptionQueryParam("after", after.map(_.asString))
      call(r.copy(uri = uri))
    def createMessage(channelId: Snowflake, message: CreateMessage): F[Message] =
      callWithFiles(route(POST, "/channels/{channel.id}/messages", channelId), message, message.files)
    def editMessage(channelId: Snowflake, messageId: Snowflake, edit: EditMessage): F[Message] =
      callWithFiles(
        route(PATCH, "/channels/{channel.id}/messages/{message.id}", channelId, messageId),
        edit,
        edit.files
      )
    def deleteMessage(channelId: Snowflake, messageId: Snowflake, reason: Option[String]): F[Unit] =
      exec(
        route(DELETE, "/channels/{channel.id}/messages/{message.id}", channelId, messageId),
        reasonHeader(reason)
      )
    def createReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): F[Unit] =
      exec(
        route(
          PUT,
          "/channels/{channel.id}/messages/{message.id}/reactions/{emoji}/@me",
          channelId,
          messageId,
          emoji
        )
      )
    def deleteOwnReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): F[Unit] =
      exec(
        route(
          DELETE,
          "/channels/{channel.id}/messages/{message.id}/reactions/{emoji}/@me",
          channelId,
          messageId,
          emoji
        )
      )

    def getGuild(guildId: Snowflake): F[Guild] = call(route(GET, "/guilds/{guild.id}", guildId))
    def getGuildChannels(guildId: Snowflake): F[List[Channel]] =
      call(route(GET, "/guilds/{guild.id}/channels", guildId))
    def getGuildMember(guildId: Snowflake, userId: Snowflake): F[GuildMember] =
      call(route(GET, "/guilds/{guild.id}/members/{user.id}", guildId, userId))
    def listGuildMembers(guildId: Snowflake, limit: Int, after: Option[Snowflake]): F[List[GuildMember]] =
      val r = route(GET, "/guilds/{guild.id}/members", guildId)
      call(
        r.copy(uri =
          r.uri.withQueryParam("limit", limit).withOptionQueryParam("after", after.map(_.asString))
        )
      )
    def getGuildRoles(guildId: Snowflake): F[List[Role]] = call(
      route(GET, "/guilds/{guild.id}/roles", guildId)
    )
    def addGuildMemberRole(
        guildId: Snowflake,
        userId: Snowflake,
        roleId: Snowflake,
        reason: Option[String]
    ): F[Unit] =
      exec(
        route(PUT, "/guilds/{guild.id}/members/{user.id}/roles/{role.id}", guildId, userId, roleId),
        reasonHeader(reason)
      )
    def removeGuildMemberRole(
        guildId: Snowflake,
        userId: Snowflake,
        roleId: Snowflake,
        reason: Option[String]
    ): F[Unit] =
      exec(
        route(DELETE, "/guilds/{guild.id}/members/{user.id}/roles/{role.id}", guildId, userId, roleId),
        reasonHeader(reason)
      )

    def createInteractionResponse(
        interactionId: Snowflake,
        token: String,
        response: InteractionResponse
    ): F[Unit] =
      execWith(route(POST, "/interactions/{interaction.id}/{token}/callback", interactionId, token), response)
    def getOriginalInteractionResponse(applicationId: Snowflake, token: String): F[Message] =
      call(route(GET, "/webhooks/{application.id}/{token}/messages/@original", applicationId, token))
    def editOriginalInteractionResponse(
        applicationId: Snowflake,
        token: String,
        edit: EditMessage
    ): F[Message] =
      callWithFiles(
        route(PATCH, "/webhooks/{application.id}/{token}/messages/@original", applicationId, token),
        edit,
        edit.files
      )
    def deleteOriginalInteractionResponse(applicationId: Snowflake, token: String): F[Unit] =
      exec(route(DELETE, "/webhooks/{application.id}/{token}/messages/@original", applicationId, token))
    def createFollowupMessage(applicationId: Snowflake, token: String, message: CreateMessage): F[Message] =
      callWithFiles(
        route(POST, "/webhooks/{application.id}/{token}", applicationId, token),
        message,
        message.files
      )

    def getGlobalCommands(applicationId: Snowflake): F[List[ApplicationCommand]] =
      call(route(GET, "/applications/{application.id}/commands", applicationId))
    def bulkOverwriteGlobalCommands(
        applicationId: Snowflake,
        commands: List[ApplicationCommand]
    ): F[List[ApplicationCommand]] =
      callWith(
        route(PUT, "/applications/{application.id}/commands", applicationId),
        commands.map(_.asJson.deepDropNullValues)
      )
    def getGuildCommands(applicationId: Snowflake, guildId: Snowflake): F[List[ApplicationCommand]] =
      call(route(GET, "/applications/{application.id}/guilds/{guild.id}/commands", applicationId, guildId))
    def bulkOverwriteGuildCommands(
        applicationId: Snowflake,
        guildId: Snowflake,
        commands: List[ApplicationCommand]
    ): F[List[ApplicationCommand]] =
      callWith(
        route(PUT, "/applications/{application.id}/guilds/{guild.id}/commands", applicationId, guildId),
        commands.map(_.asJson.deepDropNullValues)
      )
