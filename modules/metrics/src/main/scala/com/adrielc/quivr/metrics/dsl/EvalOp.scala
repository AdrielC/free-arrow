package com.adrielc.quivr.metrics
package dsl

import cats.implicits._
import cats.{Order, Show}
import com.adrielc.quivr.metrics.data.{EngagedResults, LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.Discount.Log2p1
import com.adrielc.quivr.~>|

sealed trait EvalOp[-A, +B] {
  def apply(a: A): Option[B]
}
object EvalOp {
  import MetricOp._
  import EngagementOp._
  import EngagementToRelevancy._
  import EngagementToLabel._

  sealed trait MetricOp[-A] extends EvalOp[A, Double]
  object MetricOp {

    case class Ndcg[A: IndexedLabels](g: Gain = Pow2, d: Discount = Log2p1) extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.ndcg(g, d) }
    case class AveragePrecision[A: IndexedLabels]() extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.averagePrecision }
    case class ReciprocalRank[A: IndexedLabels]()   extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.reciprocalRank }
    case class Precision[A: RelevantCount]()        extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.precision }
    case class Recall[A: RelevantCount]()           extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.recall }
    case class FScore[A: RelevantCount]()           extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.fScore }
    case class RPrecision[A: RelevantCount : ToK]() extends MetricOp[A] { def apply(v1: A): Option[Double] = v1.rPrecision }

    sealed abstract class Gain(val f: Double => Double) extends Product with Serializable { def apply(z: Label): Label = f(z) }
    object Gain {
      case object Pow2    extends Gain(powOf(2.0)) // can overflow when applied to counts of engagements
      case object Pow1p1  extends Gain(powOf(1.1)) // preferred when label is derived from a count that often exceeds 1000 (e.g. clicks)
      case object Pow1p01 extends Gain(powOf(1.01))
      case object Id      extends Gain(identity)
    }
    sealed abstract class Discount(val f: Int => Double) extends Product with Serializable { def apply(z: Index): Label = f(z) }
    object Discount {
      case object Log2p1 extends Discount(log2p1)
    }
  }
  sealed trait EngagementOp[-A, +B] extends EvalOp[A, B]
  object EngagementOp {
    sealed trait EngagementToRelevancy extends EngagementOp[EngagedResults, ResultsWithRelevant] {
      def isRelevant(f: EngagementCounts): Boolean
      final def apply(e: EngagedResults): Option[ResultsWithRelevant] = {
        e.results.toNel.toList.mapFilter { case (_, (id, eng)) => eng.flatMap(isRelevant(_).guard[Option]).as(id) }.toNel.map { nel =>
          ResultsWithRelevant(e.resultIds, nel.toNes)
        }
      }
    }
    object EngagementToRelevancy {
      case class MoreThan(e: Engagement, n: Int) extends EngagementToRelevancy {
        def isRelevant(f: EngagementCounts): Boolean = f.get(e).exists(_ > n)
      }
      case class HasAny(e: Engagement) extends EngagementToRelevancy {
        def isRelevant(f: EngagementCounts): Boolean = f.contains(e)
      }
    }
    sealed trait EngagementToLabel[-A, +B] extends EngagementOp[A, B] {
      def label(f: EngagementCounts): Option[Double]
      def apply(e: EngagedResults): Option[LabelledIndexes] = {
        val k = e.results.length
        e.results.toNel.toList
          .mapFilter { case (idx, (_, eng)) => eng.flatMap(label).map(idx -> _) }.toNel
          .map(nel => LabelledIndexes(nel.toNem, k, k))
      }
    }
    object EngagementToLabel {
      case class Count(e: Engagement) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = f.get(e).map(_.toDouble)
      }
      case class Binary(l: Labeler) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = l.getLabeler(f)
        override def apply(v1: EngagedResults): Option[LabelledIndexes] =
          super.apply(v1.map(_.map(_.mapValues(_.binarize))))
      }
      case class Plus(a: Labeler, b: Labeler) extends EngagementToLabel[EngagedResults, LabelledIndexes] { self =>
        def label(f: EngagementCounts): Option[Label] = a.getLabeler(f)
      }
      case class PercentOf(num: Engagement, den: Engagement) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = f.get(den).map { p => f.getOrElse(num, 0L) / p.toDouble }
      }
      case class WeightedCount(weights: Map[Engagement, Double]) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = weights.toList.foldMap { case (e, w) => f.get(e).map(_ * w) }
      }
    }
  }
  case class AtK[A: ToK](k: Int) extends EvalOp[A, A] {
    def apply(v1: A): Option[A] = v1.toK(k)
  }


  // metric keys are in this order
  implicit val orderEval: Order[EvalOp[_, _]] = Order.by {
    case _: EvalOp.EngagementOp[_, _] => 1
    case _: EvalOp.MetricOp[_]        => 2
    case EvalOp.AtK(_)                => 3
  }

  implicit val showEval: Show[EvalOp[Nothing, Any]] = Show.show {
    case Ndcg(`Pow2`, _)        => "ndcg"
    case Ndcg(g, _)             => s"ndcg$g"
    case Precision()            => "precision"
    case RPrecision()           => "rPrecision"
    case Recall()               => "recall"
    case FScore()               => "fScore"
    case AveragePrecision()     => "averagePrecision"
    case ReciprocalRank()       => "reciprocalRank"
    case MoreThan(e, n)         => s"${e}MoreThan$n"
    case HasAny(e)              => s"hasAny$e"
    case Count(e)               => s"count$e"
    case PercentOf(num, den)    => s"${num}Per$den"
    case WeightedCount(weights) => "wtCount_" + formatWeights(weights)
    case AtK(k)                 => s"@$k"

    case Binary(l) =>
      val binShort = new (EngagementToLabel ~>| String) {
        def apply[A, B](fab: EngagementToLabel[A, B]): String = fab match {
          case Count(e)                 => s"binary${e.shortName.capitalize}"
          case WeightedCount(weights)   => "wtBinary" + formatWeights(weights)
          case other                    => s"binary${showEval.show(other)}"
        }
      }
      l.analyze(binShort)

    case Plus(a, b) =>
      val plusShort = new (EngagementToLabel ~>| String) {
        def apply[A, B](fab: EngagementToLabel[A, B]): String = fab match {
          case Count(e)                 => s"${e.toString.toLowerCase}s"
          case WeightedCount(weights)   => "wt" + formatWeights(weights)
          case other                    => showEval.show(other)
        }
      }
      s"${a.analyze(plusShort)}Plus${b.analyze(plusShort).capitalize}"
  }

  private def formatWeights(weights: Map[Engagement, Double]): String =
    weights.toList.map { case (eng, weight) => eng.shortName + weight.toInt.toString }
      .foldSmash("", "-", "")
}