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
  }

  object Libraries {
    lazy val cats       = "org.typelevel" %% "cats-core"    % Versions.cats
    lazy val catsfree   = "org.typelevel" %% "cats-free"    % Versions.cats
    lazy val simulacrum = "org.typelevel" %% "simulacrum"   % Versions.simulacrum
    lazy val circe      = "io.circe"      %% "circe-core"   % Versions.circe
    lazy val finagle    = "com.twitter"   %% "finagle-core" % Versions.finagle
    lazy val shapeless  = "com.chuusai"   %% "shapeless"    % Versions.shapeless

    // Test
    lazy val scalaTest  = "org.scalatest"  %% "scalatest"  % Versions.scalaTest
    lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
    lazy val disclipline  = "org.typelevel"   %% "discipline-scalatest" % Versions.discipline

    // Compiler
    lazy val kindProjector    = "org.typelevel"   %%  "kind-projector"      % Versions.kindProjector
    lazy val paradise         = "org.scalamacros" %   "paradise"            % Versions.paradise cross CrossVersion.full
  }
}
