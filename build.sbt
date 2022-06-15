import Dependencies.Libraries
import sbt.{addCompilerPlugin, compilerPlugin}

name := """quivr"""

organization in ThisBuild := "com.adrielc"

inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4",
  crossScalaVersions := Seq("2.11.11", "2.12.4", "2.13.0-M2")
))

lazy val commonSettings = Seq(
  organizationName := "com.adrielc",
  scalafmtOnCompile := false,
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value,
    compilerPlugin(Libraries.paradise cross CrossVersion.patch),
    Libraries.scalaTest  % Test,
    Libraries.scalaCheck % Test
  )
)

lazy val macros = (project in file("modules/macros"))
  .settings(
    commonSettings,
    scalacOptions --= List("-Xfatal-warnings")
  )

lazy val core = (project in file("modules/core"))
  .dependsOn(macros)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.catsfree,
      Libraries.simulacrum,
      Libraries.shapeless,
      "com.slamdata" %% "matryoshka-core" % "0.21.3",
      "org.scodec" %% "scodec-bits" % "1.1.29",
      "com.twitter" %% "algebird-core" % "0.13.9",
      compilerPlugin(Libraries.kindProjector)
    )
  )

lazy val metrics = (project in file("modules/metrics"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "eu.timepit" %% "refined" % "0.9.17",
      "eu.timepit" %% "refined-cats" % "0.9.17",
      "com.slamdata" %% "matryoshka-core" % "0.21.3",
      "com.lihaoyi" %% "fastparse" % "2.3.0",
      compilerPlugin(Libraries.kindProjector)
    )
  )


lazy val finagle = (project in file("modules/finagle"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies += Libraries.finagle
  )



lazy val zio = (project in file("modules/zio"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies ++= List(
      Libraries.fs2,
      Libraries.zioStreams,
      Libraries.zioInterop,

//      Libraries.zioConfig,
//      Libraries.zioTest,
//      Libraries.zioTestSbt,
      Libraries.pprint
    )
  )

lazy val akka = (project in file("modules/akka"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.8",
    scalacOptions --= List("-Xfatal-warnings")
  )


val z = new {

  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  lazy val json4s =  "org.json4s" %% "json4s-jackson" % "3.6.6"

  lazy val cats = "org.typelevel" %% "cats-core" % "2.1.1"
  lazy val catsEffect =  "org.typelevel" %% "cats-effect" % "2.2.0"
  val commonsIo = "commons-io" % "commons-io" % "2.4"
  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val magnolia = "com.propensive" %% "magnolia" % "0.17.0"

  lazy val avro = "org.apache.avro" % "avro" % "1.9.1"
  lazy val snappy = "org.xerial.snappy" % "snappy-java" % "1.1.7.3"

  lazy val Http4sVersion = "0.21.3"
  lazy val http4sBlazeServer = "org.http4s"  %% "http4s-blaze-server" % Http4sVersion
  lazy val http4sBlazeClient = "org.http4s"  %% "http4s-blaze-client" % Http4sVersion
  lazy val http4sDsl = "org.http4s"      %% "http4s-dsl"          % Http4sVersion

  lazy val zioVersion = "1.0.1"

  lazy val zio = "dev.zio" %% "zio" %  zioVersion
  lazy val `zio-streams` = "dev.zio" %% "zio-streams" % zioVersion
  // lazy val `zio-kafka` = "dev.zio" %% "zio-kafka"   % `zio-kafka-version`
  lazy val `zio-test` = "dev.zio" %% "zio-test" % zioVersion % "test"
  lazy val `zio-test-sbt` = "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
  lazy val `zio-interop-shared` = "dev.zio" %% "zio-interop-shared" % zioVersion
  // lazy val `zio-interop-cats` = "dev.zio" %% "zio-interop-cats" % `zio-interop`

  val uzhttp = "org.polynote" %% "uzhttp" % "0.2.5"
  val sttpVersion = "2.2.9"
  val sttp = "com.softwaremill.sttp.client" %% "core" % sttpVersion
  val sttpzio  = "com.softwaremill.sttp.client" %% "async-http-client-backend-zio" % sttpVersion
  val sttpziostreams  = "com.softwaremill.sttp.client" %% "async-http-client-backend-zio-streams" % sttpVersion


  /*
  lazy val `http4s1` = (project in file ("http4s1"))
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= Seq(
      http4sBlazeServer,
      http4sBlazeClient,
      http4sDsl,
      catsEffect,
      scalaXml,
      `zio-test`
    ))
  */




  /*
  lazy val `avro-magnolia` = (project in file ("avro-magnolia"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    commonsIo,
    logback,
    scalaTest,
    json4s,
    avro,
    snappy,
    magnolia,
  )
  )
  lazy val `streams` = (project in file ("streams"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    http4sBlazeServer,
    http4sBlazeClient,
    http4sDsl,
    catsEffect,
    scalaXml,
    `zio-kafka`,
    `zio-streams`,
    `zio-test`
  ))
  */
}

val libs = org.typelevel.libraries
  .add(name = "cats", version = "2.1.1")
  .add(name = "cats-effect", version = "2.1.2")
  .add(name = "fs2", version = "2.3.0")
  .add(name = "http4s", version = "0.21.2")
  .add(name = "circe", version = "0.13.0", org = "io.circe", "circe-core", "circe-generic", "circe-generic-extras", "circe-parser", "circe-fs2")
  .add(name = "spire", version = "0.17.0-M1", org = "org.typelevel", "spire")
  .add(name = "http4s-jdk-http-client", version = "0.2.0", org = "org.http4s", "http4s-jdk-http-client")

lazy val discocat = (project in file("modules/quasar"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    libs.dependencies(
      "cats-core",
      "cats-effect",
      "fs2-core",
      "circe-core",
      "circe-generic",
      "circe-generic-extras",
      "circe-parser",
      "circe-fs2",
      "http4s-jdk-http-client",
      "spire",
    ),
    addCompilerPlugin(Libraries.kindProjector),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  )


val zioStream = (project in file ("modules/uzsttp"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    z.uzhttp,
    z.`zio-streams`,
    z.sttp,
    z.sttpzio,
    z.zio,
    z.`zio-test`,
    z.`zio-test-sbt`,
    //    sttpziostreams,
    z.catsEffect,
    z.scalaXml,
    z.`zio-test`
  ))

lazy val quivr = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(macros, core, zio, akka, zioStream)

