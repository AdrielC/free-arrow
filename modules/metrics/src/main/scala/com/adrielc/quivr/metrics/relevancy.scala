package com.adrielc.quivr.metrics

import cats.{Contravariant}
import shapeless.Nat
import shapeless.nat._
import simulacrum.typeclass
import cats.syntax.all._
import shapeless.ops.nat.ToInt

object relevancy {

  @typeclass trait Relevancy[Rel] {
    def gainValue(r: Rel): Option[Double]
    def isJudged(a: Rel): Boolean = gainValue(a).isDefined
    def isRel(a: Rel): Boolean = gainValue(a).exists(_ > 0)
    def gainOrZero(r: Rel): Double = gainValue(r).getOrElse(0.0)
  }
  object Relevancy {
    implicit val relevancyOptBool: Relevancy[Option[Boolean]] =
      _.map(if(_) 1.0 else 0.0)

    implicit def relevancyOptNum[N: Numeric]: Relevancy[Option[N]] =
      _.map(implicitly[Numeric[N]].toDouble)

    implicit def relevancySABRel: Relevancy[SABRel] =  {
      case judged: Judged[_] => Some(judged.gain.toDouble)
      case Unjudged => None
    }
    implicit def relevancyNonOpt[A](implicit R: Relevancy[Option[A]]): Relevancy[A] = R.contramap(Option(_))
    implicit val contravariant: Contravariant[Relevancy] = new Contravariant[Relevancy] {
      def contramap[A, B](fa: Relevancy[A])(f: B => A): Relevancy[B] = a => fa.gainValue(f(a))
    }
  }


  sealed trait SABRel extends Product with Serializable
  sealed abstract class Judged[N <: Nat: ToInt] extends SABRel {lazy val gain: Int = ToInt[N].apply() }
  object Judged {
    case object SRel extends Judged[_3] // Highly Relevant
    case object ARel extends Judged[_2] // Relevant
    case object BRel extends Judged[_1] // Partially Relevant
    case object NRel extends Judged[_0] // Non Relevant

    val S: Judged[_3] = SRel
    val A: Judged[_2] = ARel
    val B: Judged[_1] = BRel
    val N: Judged[_0] = NRel
  }
  case object Unjudged extends SABRel
}
