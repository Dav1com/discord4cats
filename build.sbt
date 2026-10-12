import org.scalajs.linker.interface.ModuleKind

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "cl.davicom"
ThisBuild / organizationName := "davicom"
ThisBuild / startYear := Some(2026)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("davicom", "davicom"))

ThisBuild / scalaVersion := "3.3.8"
ThisBuild / crossScalaVersions := Seq("3.3.8")
ThisBuild / tlCiReleaseBranches := Seq.empty
ThisBuild / tlJdkRelease := Some(17)
ThisBuild / tlFatalWarnings := false

// CI needs the same native libraries as the devcontainer: s2n-tls (TLS on Scala Native) and libdave (DAVE E2EE).
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("25"))
ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Run(
    List(
      "sudo apt-get update",
      "sudo apt-get install -y clang lld cmake libssl-dev nlohmann-json3-dev",
      "scripts/install-s2n.sh",
      "scripts/install-libdave.sh",
      "npm ci"
    ),
    name = Some("Install native libraries and Node.js dependencies")
  )
)

val V = new {
  val catsEffect = "3.7.1"
  val fs2 = "3.14.0"
  val circe = "0.14.17"
  val http4s = "0.23.38"
  val scodecBits = "1.2.5"
  val log4cats = "2.8.0"
  val munit = "1.3.6"
  val munitCE = "2.2.1"
}

lazy val root = tlCrossRootProject.aggregate(core, rest, gateway, commands, voice, examples)

lazy val commonJsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

lazy val testDeps = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % V.munit % Test,
    "org.typelevel" %%% "munit-cats-effect" % V.munitCE % Test,
    "org.typelevel" %%% "cats-effect-testkit" % V.catsEffect % Test
  )
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "discord-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "io.circe" %%% "circe-core" % V.circe,
      "io.circe" %%% "circe-parser" % V.circe,
      "org.scodec" %%% "scodec-bits" % V.scodecBits
    ),
    testDeps
  )
  .jsSettings(commonJsSettings)

lazy val rest = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/rest"))
  .dependsOn(core)
  .settings(
    name := "discord-rest",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % V.catsEffect,
      "co.fs2" %%% "fs2-scodec" % V.fs2,
      "org.http4s" %%% "http4s-client" % V.http4s,
      "org.http4s" %%% "http4s-circe" % V.http4s
    ),
    testDeps,
    libraryDependencies += "org.http4s" %%% "http4s-dsl" % V.http4s % Test
  )
  .jsSettings(commonJsSettings)

lazy val gateway = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/gateway"))
  .dependsOn(rest)
  .settings(
    name := "discord-gateway",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % V.fs2,
      "co.fs2" %%% "fs2-io" % V.fs2,
      "org.typelevel" %%% "log4cats-core" % V.log4cats,
      "org.typelevel" %%% "log4cats-noop" % V.log4cats
    ),
    testDeps
  )
  .jsSettings(commonJsSettings)

lazy val commands = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/commands"))
  .dependsOn(gateway)
  .settings(
    name := "discord-commands",
    testDeps,
    libraryDependencies += "org.http4s" %%% "http4s-dsl" % V.http4s % Test
  )
  .jsSettings(commonJsSettings)

lazy val voice = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/voice"))
  .dependsOn(gateway % "compile->compile;test->test")
  .settings(name := "discord-voice", testDeps)
  // Panama FFM (used to bind libdave) is final since JDK 22.
  .jvmSettings(
    tlJdkRelease := Some(22),
    Test / fork := true,
    Test / javaOptions += "--enable-native-access=ALL-UNNAMED"
  )
  .jsSettings(commonJsSettings)

lazy val examples = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(commands, voice)
  .settings(
    name := "discord-examples",
    libraryDependencies += "org.http4s" %%% "http4s-ember-client" % V.http4s,
    Compile / mainClass := Some("discord.examples.Main")
  )
  .jvmSettings(
    tlJdkRelease := Some(22),
    run / fork := true,
    run / javaOptions += "--enable-native-access=ALL-UNNAMED",
    run / connectInput := true,
    // Relative paths like ./audio resolve against the repository root.
    run / baseDirectory := (ThisBuild / baseDirectory).value
  )
  .jsSettings(commonJsSettings, scalaJSUseMainModuleInitializer := true)
