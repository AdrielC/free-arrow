package com.adrielc.quivr.metrics
package dsl

import cats.implicits._
import cats.{Order, Show}
import com.adrielc.quivr.metrics.data.{EngagedResults, LabelledIndexes, RelevanceCounts, ResultsWithRelevant}
import com.adrielc.quivr.~>|

sealed trait EvalOp[-A, +B] extends Product with Serializable {
  def apply(a: A): Option[B]
}
object EvalOp {
  import Metric._
  import EngagementOp._
  import EngagementToRelevancy._
  import EngagementToLabel._

  sealed trait Metric[-A] extends EvalOp[A, Double]
  object Metric {

    case class Dcg(gain: Gain)                      extends RankingMetric
    case class Ndcg(gain: Gain)                     extends RankingMetric
    case object AveragePrecision                    extends RankingMetric
    case object ReciprocalRank                      extends RankingMetric
    case object RPrecision                          extends RankingMetric
    case object Precision                           extends RetrievalMetric
    case object Recall                              extends RetrievalMetric
    case object FScore                              extends RetrievalMetric

    sealed trait RankingMetric extends Metric[LabelledIndexes] {
      def apply(a: LabelledIndexes): Option[Label] = this match {
        case Dcg(g)           => a.dcg(g.f)
        case Ndcg(g)          => a.ndcg(g.f)
        case AveragePrecision => a.averagePrecision
        case ReciprocalRank   => a.reciprocalRank
        case RPrecision       => a.rPrecision
      }
    }
    sealed trait RetrievalMetric extends Metric[RelevanceCounts] {
      def apply(a: RelevanceCounts): Option[Label] = this match {
        case Precision  => a.precision
        case Recall     => a.recall
        case FScore     => a.fScore
      }
    }


    sealed abstract class Gain(val f: Double => Double)
    object Gain {
      case object Pow2    extends Gain(pow2) // can overflow when applied to counts of engagements
      case object Pow1p1  extends Gain(powOf(1.1)) // preferred when label is derived from a count that often exceeds 1000 (e.g. clicks)
      case object Pow1p01 extends Gain(powOf(1.01))
      case object Id      extends Gain(identity)
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
          .map(nel => LabelledIndexes(nel.toNem, k))
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
    case _: EvalOp.Metric[_]        => 2
    case EvalOp.AtK(_)                => 3
  }

  implicit val showEval: Show[EvalOp[Nothing, Any]] = Show.show {
    case Dcg(`Pow2`)            => "dcg"
    case Ndcg(`Pow2`)           => "ndcg"
    case Dcg(g)                 => s"dcg-${g.toString.toLowerCase}"
    case Ndcg(g)                => s"ndcg-${g.toString.toLowerCase}"
    case Precision              => "precision"
    case RPrecision             => "rPrecision"
    case Recall                 => "recall"
    case FScore                 => "f1"
    case AveragePrecision       => "map"
    case ReciprocalRank         => "mrr"
    case MoreThan(e, n)         => s"${e}MoreThan$n"
    case HasAny(e)              => s"hasAny$e"
    case Count(e)               => s"count$e"
    case PercentOf(num, den)    => s"${num}Per$den"
    case WeightedCount(weights) => "wtCount-" + formatWeights(weights)
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