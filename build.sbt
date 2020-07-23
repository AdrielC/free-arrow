import Dependencies.Libraries

name := """free-arrow"""

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
      Libraries.simulacrum,
      compilerPlugin(Libraries.kindProjector)
    )
  )

lazy val zio = (project in file("modules/zio"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies += "dev.zio" %% "zio" % "1.0.0-RC21-2"
  )

lazy val akka = (project in file("modules/akka"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.8",
    scalacOptions --= List("-Xfatal-warnings")
  )


lazy val `free-arrow` = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(macros, core, zio, akka)