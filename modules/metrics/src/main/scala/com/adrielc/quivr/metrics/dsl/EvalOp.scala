package com.adrielc.quivr.metrics
package dsl

import cats.implicits._
import cats.{Order, Show}
import com.adrielc.quivr.metrics.data.{LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.LabelOp.Pow.{P101, P11, P2}
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

      final def apply(e: EngagedResults): Option[ResultsWithRelevant] = {
        e.results.toNel.toList.mapFilter { case (_, (id, eng)) => isRelevant(eng).guard[Option].as(id) }.toNel.map { nel =>
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

      def apply(e: EngagedResults): Option[LabelledIndexes] =
        e.results.toNel.toList
          .mapFilter { case (idx, (_, eng)) => label(eng).map(idx -> _) }.toNel
          .map(nel => LabelledIndexes(nel.toNem, e.results.length))
    }

    object EngagementToLabel {

      case class Count(e: Engagement) extends EngagementToLabel[EngagedResults, LabelledIndexes] {
        def label(f: EngagementCounts): Option[Label] = f.get(e).map(_.toDouble)
      }
      case class Binary(l: Labeler) extends EngagementToLabel[EngagedResults, LabelledIndexes] {

        def label(f: EngagementCounts): Option[Label] = l.getLabeler(f)

        override def apply(v1: EngagedResults): Option[LabelledIndexes] =
          super.apply(v1.map(_.mapValues(_.binarize)))
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

    sealed trait Pow extends LabelOp[LabelledIndexes, LabelledIndexes] with Product with Serializable {

      def pow(d: Double): Double

      def apply(a: LabelledIndexes): Option[LabelledIndexes] = Some(a.copy(labels = a.labels.map(pow)))
    }
    object Pow {
      case object P2    extends Pow { // can overflow when applied to counts of engagements
        def pow(d: Label): Label = powOf(2.0)(d)
      }
      case object P11    extends Pow { // preferred when label is derived from a count that often exceeds 1000 (e.g. clicks)
        def pow(d: Label): Label = powOf(1.1)(d)
      }
      case object P101   extends Pow {
        def pow(d: Label): Label = powOf(1.01)(d)
      }
    }
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, A] {
    def apply(v1: A): Option[A] = v1.toK(k)
  }


  implicit val orderEval: Order[EvalOp[_, _]] = Order.by {
    case _: EvalOp.EngagementOp[_, _] => 1
    case _: EvalOp.RankingMetric[_]   => 2
    case EvalOp.AtK(_)                => 3
    case _: LabelOp[_, _]             => 4
  }

  implicit val showEval: Show[EvalOp[Nothing, Any]] = Show.show {
    case Ndcg()                 => "ndcg"
    case Precision()            => "precision"
    case Recall()               => "recall"
    case MoreThan(e, n)         => s"${e}MoreThan$n"
    case HasAny(e)              => s"hasAny$e"
    case Count(e)               => s"count$e"
    case PercentOf(num, den)    => s"${num}Per$den"
    case WeightedCount(weights) => "wtCount_" + formatWeights(weights)
    case AtK(k)                 => s"@$k"
    case P2                     => "p2"
    case P11                    => "p11"
    case P101                   => "p101"

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