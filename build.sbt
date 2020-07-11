import Dependencies.Libraries

name := """free-arrow"""


organization in ThisBuild := "com.adrielc"

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4", "2.13.0-M2")

inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4"
))

lazy val commonSettings = Seq(
  organizationName := "com.adrielc",
  scalafmtOnCompile := true,
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value,
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.circe,
    Libraries.shapeless,
    Libraries.simulacrum,
    Libraries.scalaTest  % Test,
    Libraries.scalaCheck % Test,
    compilerPlugin(Libraries.kindProjector),
    compilerPlugin(Libraries.betterMonadicFor),
    compilerPlugin(Libraries.paradise cross CrossVersion.patch)
  )
)

lazy val `free-arrow` = project in file(".") settings(commonSettings: _*)