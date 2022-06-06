import Dependencies.Libraries
import sbt.compilerPlugin

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
    libraryDependencies += "dev.zio" %% "zio" % "1.0.0-RC21-2",
    libraryDependencies += "dev.zio" %% "zio-interop-cats" % "2.1.4.0-RC17",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4",
    libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.7.1"
  )

lazy val akka = (project in file("modules/akka"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.8",
    scalacOptions --= List("-Xfatal-warnings")
  )


lazy val quivr = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(macros, core, zio, akka)