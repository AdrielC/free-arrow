import Dependencies.Libraries

name := """free-arrow"""

organization in ThisBuild := "com.adrielc"

scalaVersion in ThisBuild := "2.12.10"

crossScalaVersions in ThisBuild := Seq("2.12.10", "2.13.1")

lazy val commonSettings = Seq(
  organizationName := "com.adrielc",
  scalafmtOnCompile := true,
  libraryDependencies ++= Seq(
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.scalaTest  % Test,
    Libraries.scalaCheck % Test,
    compilerPlugin(Libraries.kindProjector),
    compilerPlugin(Libraries.betterMonadicFor)
  )
)

lazy val `free-arrow-root` =
  (project in file("."))
    .aggregate(`free-arrow-core`)

lazy val `free-arrow-core` = project
  .in(file("core"))
  .settings(commonSettings: _*)
