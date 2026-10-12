# discord-scala

A Discord API library for Scala 3 on the Typelevel stack (cats-effect, fs2, http4s, circe), for the **JVM**, **Scala.js (Node.js)** and **Scala Native**.

- **REST**: typed endpoints for messages, channels, guilds, interactions and application commands, with Discord's rate limits handled for you.
- **Gateway**: events as an fs2 `Stream`. Heartbeats, resume and reconnects are automatic.
- **Slash commands**: declare a command and its typed options in one place; the router decodes the options, dispatches interactions and defers slow handlers.
- **Voice**: join voice channels and stream Opus audio, end-to-end encrypted with DAVE (which Discord requires for voice since March 2026).

## Modules

| Module | Artifact | What it gives you |
| --- | --- | --- |
| core | `discord-core` | Models (`Snowflake`, `Message`, `Interaction`, ...) and circe codecs |
| rest | `discord-rest` | `DiscordRest[F]` on top of any http4s `Client[F]` |
| gateway | `discord-gateway` | `Gateway[F]`, plus the cross-platform WebSocket client it runs on |
| commands | `discord-commands` | `Commands`, `Opt`, `CommandRouter` |
| voice | `discord-voice` | `Voice.join`, `VoiceConnection.play`, `OggOpus` |

```scala
libraryDependencies ++= Seq(
  "cl.davicom" %%% "discord-commands" % "0.1.0", // pulls in core, rest and gateway
  "cl.davicom" %%% "discord-voice" % "0.1.0",
  "org.http4s" %%% "http4s-ember-client" % "0.23.38" // or any other http4s client
)
```

Nothing is published yet. For now, use `sbt publishLocal`.

## Quick start

```scala
import cats.effect.{IO, IOApp}
import discord.commands.*
import discord.gateway.*
import discord.model.*
import discord.rest.*
import org.http4s.ember.client.EmberClientBuilder

object MyBot extends IOApp.Simple:
  val token = "..." // read it from the environment, e.g. cats.effect.std.Env

  def run: IO[Unit] =
    (for
      http <- EmberClientBuilder.default[IO].build
      rest <- cats.effect.Resource.eval(DiscordRest[IO](http, token))
      gateway <- Gateway[IO](rest, GatewayConfig(token, Intents.NonPrivileged))
    yield (rest, gateway)).use { (rest, gateway) =>
      val cmds = Commands[IO]
      val greet = cmds.slash("greet", "Greets someone", Opt.user("who", "Who to greet")) { (ctx, who) =>
        ctx.reply(s"Hello ${who.mention}!")
      }
      val router = CommandRouter(rest, List(greet))

      gateway.ready.flatMap(r => router.register(r.application.id)) >>
        router.run(gateway.events).compile.drain
    }
```

### Events

`gateway.events` is a `Stream[F, GatewayEvent]`, and each call is a separate subscription. If you send a command and then wait for the events it causes, use `gateway.subscribe` (a `Resource`) so nothing is missed in between. Event types the library doesn't model yet come through as `GatewayEvent.Unknown(name, json, error)`, never as a failure.

```scala
gateway.events.collect { case GatewayEvent.MessageCreate(m) if m.content == "!ping" => m }
  .evalMap(m => rest.createMessage(m.channelId, CreateMessage.text("Pong!").replyTo(m)))
```

### Slash commands

- Option builders: `Opt.string`, `int`, `number`, `bool`, `user`, `member`, `channel`, `role`, `mentionable` and `attachment`. Combinators: `.optional`, `.withDefault(x)`, `.map`, `.emap`.
- `cmds.slash(...)` takes up to six options, and the handler receives them already typed.
- `cmds.group(...)(sub1, cmds.subgroup(...)(sub2))` builds subcommands. `cmds.user` and `cmds.message` build context-menu commands.
- `.autocomplete("option")(typed => F[List[Choice]])` adds autocomplete, and `.requirePermissions(...)` / `.guildOnly` restrict who can use a command.
- `ctx.reply` does the right thing for where the interaction is: the first reply, an edit of a deferred reply, or a follow-up. If a handler hasn't answered after 2.5s, the router defers for it, so you don't hit Discord's 3-second limit.
- `router.register(appId, Some(guildId))` registers commands in a single guild, where changes show up instantly. Use it during development.

### Voice

```scala
Voice.join(gateway, guildId, channelId).use { vc =>
  vc.play(Files[IO].readAll(Path("song.opus")).through(OggOpus.frames))
}
```

- The bot needs the `GuildVoiceStates` intent.
- `play` takes 20ms Opus frames and paces them in real time. `OggOpus.frames` reads `.opus` files in pure Scala. To convert other formats: `ffmpeg -i in.mp3 -c:a libopus -b:a 96k -ar 48000 -ac 2 out.opus`.
- Transport encryption is `aead_aes256_gcm_rtpsize`. It uses `javax.crypto` on the JVM, Node's `crypto` on Scala.js and OpenSSL's libcrypto on Scala Native.
- **DAVE** (end-to-end encryption) is negotiated automatically. `vc.daveProtocolVersion` is `0` in stage channels, which are exempt.

## Platform requirements

| | JVM | Scala.js | Scala Native |
| --- | --- | --- | --- |
| core, rest, gateway, commands | JDK 17+ | Node.js | `libs2n` (TLS, needed by fs2) |
| voice | JDK 22+ and `--enable-native-access=ALL-UNNAMED`, plus `libdave` | Node.js and `npm install @snazzah/davey` | `libdave`, `libcrypto` |

**Where DAVE comes from.** Discord publishes [libdave](https://github.com/discord/libdave) as a C++ library with a C API. On the JVM it is bound through the Panama FFM API, and on Scala Native through `@extern`. Discord doesn't publish libdave for JavaScript runtimes, so on Node.js the voice module uses [`@snazzah/davey`](https://github.com/Snazzah/davey), the DAVE implementation discord.js uses.

`scripts/install-libdave.sh` builds and installs libdave and its MLS dependency (`scripts/install-s2n.sh` does the same for s2n). The devcontainer already includes both. On the JVM, set `DAVE_LIBRARY_PATH` if `libdave.so` isn't on the system library path.

## Examples

```sh
export DISCORD_TOKEN=...
export DISCORD_GUILD_ID=...   # optional: register commands in this guild only (instant)
sbt "examplesJVM/run ping"     # !ping and /ping (needs the Message Content intent for !ping)
sbt "examplesJVM/run voice"    # /join, /play, /stop, /leave; plays .opus files from $AUDIO_DIR (default ./audio)
sbt "examplesJVM/run voice-test" # joins the busiest voice channel in DISCORD_GUILD_ID, plays one file, leaves
sbt "examplesNative/run voice" # same bot, as a native binary
npm ci && EXAMPLE=voice sbt examplesJS/run   # Scala.js `run` takes no arguments, so use EXAMPLE
```

## Development

```sh
sbt test                 # every module on JVM, Scala.js and Scala Native
sbt rootJVM/test         # one platform
DISCORD_LIVE_TESTS=1 sbt gatewayJVM/test   # also connects to the real gateway (no token needed)
sbt prePR                # format, headers and workflow regeneration (sbt-typelevel)
```

The tests run without Discord. REST runs against a fake http4s app, the gateway and voice against an in-memory WebSocket, and the WebSocket client against a real loopback socket. The DAVE tests exercise the real libdave/davey bindings on each platform.

## Not done yet

Sharding, zlib transport compression, receiving voice, PCM input (libopus encoding), video, a cache, full endpoint coverage, and publishing to Maven Central. The voice encryption mode `aead_xchacha20_poly1305_rtpsize` isn't implemented either; Discord's voice servers always offer AES-GCM.

## License

Apache 2.0
