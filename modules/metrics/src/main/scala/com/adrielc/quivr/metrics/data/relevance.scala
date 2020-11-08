package com.adrielc.quivr.metrics.data

import cats.Eq
import cats.kernel.CommutativeMonoid

object relevance {

  sealed trait Relevance {
    import Relevance._
    def gain: Option[Double] = this match {
      case Relevance.Unjudged => None
      case GradedRel(l)       => Some(l)
      case BinaryRel(r)       => Some(if(r) 1.0 else 0.0)
    }
  }
  object Relevance {

    def label(label: Double): Relevance = GradedRel(label)
    def judge(isRel: Boolean): Relevance = BinaryRel(isRel)
    val irrelevant: Relevance = BinaryRel(false)
    val zero: Relevance = label(0.0)
    val unjudged: Relevance = Unjudged

    private case object Unjudged                              extends Relevance
    sealed trait Judged                                       extends Relevance
    private final case class GradedRel(label: Double)         extends Judged
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
  sealed abstract class SABN(val gain: Option[Double]) extends Product with Serializable
  object SABN {
    case object HighlyRelevant    extends SABN(Some(3))
    case object Relevant          extends SABN(Some(2))
    case object PartiallyRelevant extends SABN(Some(1))
    case object Irrelevant        extends SABN(Some(0))
    case object Unjudged          extends SABN(None)
  }
}
