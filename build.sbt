import Dependencies.Libraries

name := """free-arrow"""

organization in ThisBuild := "com.adrielc"

scalaVersion in ThisBuild := "2.12.10"

crossScalaVersions in ThisBuild := Seq("2.12.10", "2.13.1")

//addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

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
    compilerPlugin(Libraries.betterMonadicFor),
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
)

lazy val `free-arrow` = project in file(".") settings(commonSettings: _*)