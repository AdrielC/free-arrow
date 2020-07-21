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
    compilerPlugin(Libraries.paradise cross CrossVersion.patch)
  )
)

lazy val macros = (project in file("macros"))
  .settings(
    commonSettings,
    scalacOptions --= List("-Xfatal-warnings")
  )

lazy val core = (project in file("core"))
  .dependsOn(macros)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      Libraries.cats,
      Libraries.simulacrum,
      Libraries.scalaTest  % Test,
      Libraries.scalaCheck % Test,
      "com.codecommit" %% "skolems" % "0.1.2",
      compilerPlugin(Libraries.kindProjector)
    )
  )

lazy val `free-arrow` = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(macros, core)