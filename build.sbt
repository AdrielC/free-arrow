import Dependencies.Libraries

name := """arrows"""

organization in ThisBuild := "com.adrielc"

scalaVersion in ThisBuild := "2.13.1"

crossScalaVersions in ThisBuild := Seq("2.13.1", "2.12.10")

lazy val commonSettings = Seq(
  organizationName := "com.adrielc",
  scalafmtOnCompile := true,
  libraryDependencies ++= Seq(
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.circe,
    Libraries.simulacrum,
    Libraries.scalaTest  % Test,
    Libraries.scalaCheck % Test,
    compilerPlugin(Libraries.kindProjector),
    compilerPlugin(Libraries.betterMonadicFor)
  )
)

lazy val `free-arrow` = project in file(".") settings(commonSettings: _*)