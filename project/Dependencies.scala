import sbt._

object Dependencies {

  object Versions {
    val cats        = "2.0.0"
    val circe       = "0.12.3"
    val simulacrum  = "1.0.0"
    val finagle     = "20.8.1"
    val shapeless   = "2.3.3"

    // Test
    val scalaTest  = "3.0.8"
    val scalaCheck = "1.14.2"
    val discipline = "1.0.1"

    // Compiler
    val kindProjector    = "0.10.3"
    val paradise         = "2.1.1"

    val fs2        = "1.0.5"
    val catsEffect = "2.0.0"
    val zio        = "1.0.0-RC18-2"
    val zioInterop = "2.0.0.0-RC12"
  }

  object Libraries {
    lazy val cats       = "org.typelevel" %% "cats-core"    % Versions.cats
    lazy val catsfree   = "org.typelevel" %% "cats-free"    % Versions.cats
    lazy val catsEffect = "org.typelevel" %% "cats-effect" % Versions.cats
    lazy val simulacrum = "org.typelevel" %% "simulacrum"   % Versions.simulacrum

    lazy val fs2        = "co.fs2" %% "fs2-io" % Versions.fs2

    lazy val circe      = "io.circe"      %% "circe-core"   % Versions.circe
    lazy val finagle    = "com.twitter"   %% "finagle-core" % Versions.finagle
    lazy val shapeless  = "com.chuusai"   %% "shapeless"    % Versions.shapeless

    lazy val zio        = "dev.zio" %% "zio" % Versions.zio
    lazy val zioInterop = "dev.zio" %% "zio-interop-cats" % Versions.zioInterop
    lazy val zioStreams = "dev.zio" %% "zio-streams" % Versions.zio
    lazy val zioConfig  = "dev.zio" %% "zio-config-magnolia" % "2.0.4"


    // Test
    lazy val zioTest    = "dev.zio"     %% "zio-test"     % Versions.zio % Test
    lazy val zioTestSbt = "dev.zio"     %% "zio-test-sbt" % Versions.zio % Test
    lazy val pprint     = "com.lihaoyi" %% "pprint"       % "0.5.6" % Test

    lazy val scalaTest  = "org.scalatest"  %% "scalatest"  % Versions.scalaTest
    lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
    lazy val discipline = "org.typelevel"   %% "discipline-scalatest" % Versions.discipline

    // Compiler
    lazy val kindProjector    = "org.typelevel"   %%  "kind-projector"      % Versions.kindProjector
    lazy val paradise         = "org.scalamacros" %   "paradise"            % Versions.paradise cross CrossVersion.full
  }
}
