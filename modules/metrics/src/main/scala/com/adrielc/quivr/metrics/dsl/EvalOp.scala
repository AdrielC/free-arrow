package com.adrielc.quivr.metrics
package dsl

import cats.implicits._
import cats.{Order, Show}
import com.adrielc.quivr.metrics.data.{Engagement, LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.LabelOp.Pow.{OnePointOne, OnePointZOne, Two}
import com.adrielc.quivr.~>|

sealed trait EvalOp[-A, +B] {
  def apply(a: A): Option[B]
}
object EvalOp {
  import RankingMetric._
  import EngagementOp._
  import EngagementToRelevancy._
  import EngagementToLabel._

  sealed trait RankingMetric[-A] extends EvalOp[A, Double]
  object RankingMetric {

    case class Ndcg[A: IndexedLabels]() extends RankingMetric[A] { def apply(v1: A): Option[Double] = v1.ndcg }
    object Ndcg { def apply[A: IndexedLabels](a: A): Option[Double] = Ndcg[A]().apply(a) }

    case class Precision[A: RelevantCount]() extends RankingMetric[A] { def apply(v1: A): Option[Double] = v1.precision }
    object Precision { def apply[A: RelevantCount](a: A): Option[Double] = Precision[A]().apply(a) }

    case class Recall[A: RelevantCount]() extends RankingMetric[A] { def apply(v1: A): Option[Double] = v1.recall }
    object Recall { def apply[A: RelevantCount](a: A): Option[Double] = Recall[A]().apply(a) }
  }


  sealed trait EngagementOp[-A, +B] extends EvalOp[A, B]
  object EngagementOp {

    sealed trait EngagementToRelevancy extends EngagementOp[EngagedResults, ResultsWithRelevant] {

      def isRelevant(f: EngagementCounts): Boolean

      final def apply(e: EngagedResults): Option[ResultsWithRelevant] =
        ResultsWithRelevant(e._1, e._1.toList.mapFilter(r => e._2.lookup(r).filter(isRelevant).as(r)))
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

      def apply(e: EngagedResults): Option[LabelledIndexes] =
        e._1.toList.zipWithIndex
          .mapFilter { case (r, i) => e._2.lookup(r).flatMap(label).map((i + 1) -> _) }.toNel
          .map(a => LabelledIndexes(a.toNem, e._1.length))
    }

    object EngagementToLabel {

      case class Count(e: Engagement) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = f.get(e).map(_.toDouble)
      }
      case class Binary(l: Labeler) extends EngagementToLabel[EngagedResults, LabelledIndexes] {

        def label(f: EngagementCounts): Option[Label] = l.getLabeler(f)

        override def apply(v1: EngagedResults): Option[LabelledIndexes] =
          super.apply((v1._1, v1._2.map(_.mapValues(_.binarize))))
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


  sealed trait LabelOp[-A, +B] extends EvalOp[A, B]
  object LabelOp {

    sealed abstract class Pow private (e: Double) extends LabelOp[LabelledIndexes, LabelledIndexes] {

      def apply(a: LabelledIndexes): Option[LabelledIndexes] = Some(a.copy(labels = a.labels.map(pow)))

      private def pow(d: Double): Double = powOf(e)(d)
    }
    object Pow {
      case object Two           extends Pow(2.0) // can overflow when applied to counts of engagements
      case object OnePointOne   extends Pow(1.1) // preferred when label is derived from a count that often exceeds 1000 (e.g. clicks)
      case object OnePointZOne  extends Pow(1.01)
    }
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, A] {
    def apply(v1: A): Option[A] = v1.toK(k)
  }

  implicit val orderEval: Order[EvalOp[_, _]] = Order.by {
    case _: EvalOp.RankingMetric[_]   => 1
    case _: LabelOp[_, _]             => 2
    case _: EvalOp.EngagementOp[_, _] => 3
    case EvalOp.AtK(_)                => 4
  }

  implicit val showEval: Show[EvalOp[Nothing, Any]] = Show.show[EvalOp[Nothing, Any]] {
    case Ndcg()                 => "ndcg"
    case Precision()            => "precision"
    case Recall()               => "recall"
    case MoreThan(e, n)         => s"${e}MoreThan$n"
    case HasAny(e)              => s"hasAny$e"
    case Count(e)               => s"count$e"
    case PercentOf(num, den)    => s"${num}Per$den"
    case WeightedCount(weights) => "wtCount" + formatWeights(weights)
    case AtK(k)                 => s"@$k"
    case Two                    => "pow2"
    case OnePointOne            => "pow1p1"
    case OnePointZOne           => "pow1p01"

    case Binary(l) =>
      val binShort = new (EngagementToLabel ~>| String) {
        def apply[A, B](fab: EngagementToLabel[A, B]): String = fab match {
          case Count(e)                 => s"binary$e"
          case WeightedCount(weights)   => "wtBinary" + formatWeights(weights)
          case other                    => s"binary${showEval.show(other)}"
        }
      }
      l.analyze(binShort)

    case Plus(a, b) =>
      val plusShort = new (EngagementToLabel ~>| String) {
        def apply[A, B](fab: EngagementToLabel[A, B]): String = fab match {
          case Count(e) => e.toString.toLowerCase
          case WeightedCount(weights)   => "wt" + formatWeights(weights)
          case other                    => showEval.show(other)
        }
      }
      s"${a.analyze(plusShort)}Plus${b.analyze(plusShort).capitalize}"
  }

  private def formatWeights(weights: Map[Engagement, Double]): String =
    weights.toList.foldMap { case (eng, weight) => "_" + eng.toString.toLowerCase + "-" + weight.toInt.toString }
}