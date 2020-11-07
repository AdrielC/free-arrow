package com.adrielc.quivr.metrics
package data

import cats.Eq
import cats.kernel.CommutativeMonoid

object relevance {

  sealed trait Relevance {
    import Relevance._
    def gain: Option[Gain] = this match {
      case Relevance.Unjudged => None
      case GradedRel(l)       => Some(l)
      case BinaryRel(r)       => Some(if(r) 1.0 else 0.0)
    }
  }
  object Relevance {

    def label(label: Label): Relevance = GradedRel(label)
    def judge(isRel: Boolean): Relevance = BinaryRel(isRel)
    val relevant: Relevance = BinaryRel(true)
    val irrelevant: Relevance = BinaryRel(false)
    val unjudged: Relevance = Unjudged

    private case object Unjudged                              extends Relevance
    sealed trait Judged                                       extends Relevance
    private final case class GradedRel(label: Label)          extends Judged
    private final case class BinaryRel(isRelevant: Boolean)   extends Judged

    implicit val relevanceEq: Eq[Relevance] = Eq.by(_.gain)

    implicit val relevanceMonoid: CommutativeMonoid[Relevance] = CommutativeMonoid.instance(Relevance.unjudged, {
      case (Unjudged,   other)          => other
      case (other,      Unjudged)       => other
      case (BinaryRel(a), BinaryRel(b)) => BinaryRel(a || b)
      case (GradedRel(a), GradedRel(b)) => GradedRel(a + b)
      case (BinaryRel(r), GradedRel(l)) => GradedRel(if(r) 1.0 else l)
      case (GradedRel(l), BinaryRel(r)) => GradedRel(if(r) 1.0 else l)
    })
  }


  // Frequently used relevance scale in IR Evaluation
  sealed trait SABRel extends Product with Serializable {
    def gain: Option[Gain] = this match {
      case j: SABRel.Judged[_]  => Option(j.level.toDouble)
      case SABRel.Unjudged      => None
    }
  }
  object SABRel {
    import shapeless.{Nat, nat, ops}, ops.nat.ToInt, nat._

    sealed abstract class Judged[N <: Nat: ToInt] extends SABRel {
      lazy val level: Int = ToInt[N].apply()
    }
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
}
