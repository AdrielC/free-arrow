package com.adrielc.quivr.metrics.data

import cats.Monoid
import com.adrielc.quivr.metrics.result.{BinaryRelevancy, Relevancy}

object relevance {

  sealed trait Relevance {
    import Relevance._
    def gain: Option[Double] = this match {
      case Relevance.Unjudged     => None
      case GradedRel(label)       => Some(label)
      case BinaryRel(isRelevant)  => Some(if(isRelevant) 1.0 else 0.0)
    }
  }
  object Relevance {

    def grade(grade: Double): GradedRel = GradedRel(grade)
    def judge(isRel: Boolean): BinaryRel = BinaryRel(isRel)
    val irrelevant: BinaryRel = BinaryRel(false)
    val zero: GradedRel = grade(0.0)
    val unjudged: Relevance = Unjudged


    case object Unjudged extends Relevance

    sealed trait Judged extends Relevance {
      def gainValue: Double = this match {
        case GradedRel(l)   => l
        case BinaryRel(r)   => if(r) 1.0 else 0.0
      }
    }

    final case class GradedRel(label: Double) extends Judged
    object GradedRel {
      implicit val gradedRelMonoid: Monoid[GradedRel] = Monoid.instance(zero, (a, b) => grade(a.label + b.label))
      implicit def gradedRelRelevance: Relevancy[GradedRel] = a => {
        println("graded\t\t" + a)
        println("graded\t\t" + a.gainValue)
        a.gainValue
      }
    }

    final case class BinaryRel(isRelevant: Boolean) extends Judged
    object BinaryRel {
      implicit val binaryMonoid: Monoid[BinaryRel] = Monoid.instance(irrelevant, (a, b) => judge(a.isRelevant || b.isRelevant))
      implicit def binaryRelRelevance: BinaryRelevancy[BinaryRel] = a => {
        println("binary rel" + a.gain.getOrElse(0.0))
        println("binary rel" + a)
        a.isRelevant
      }
    }

    implicit val relevance: BinaryRelevancy[Relevance] = new BinaryRelevancy[Relevance] {
      override def isRel(a: Relevance): Boolean = a.gain.exists(_ > 0)
    }
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
