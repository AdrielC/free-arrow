package com.adrielc.quivr.metrics
package dsl


import cats.data.{Kleisli, NonEmptyMap, NonEmptySet, WriterT}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data._
import com.adrielc.quivr.metrics.function.Gain
import com.adrielc.quivr.~~>
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._


sealed trait EvalOp[-A, +B] extends Product with Serializable {
  def apply(a: A): Option[B]
}
object EvalOp {

  sealed abstract class MetricOp[-A, +B](f: A => Option[B]) extends EvalOp[A, B] {
    final def apply(a: A): Option[B] = f(a)
  }
  object MetricOp {
    case class Ndcg[A: LabelledSet](gain: Gain)           extends MetricOp[A, Double](_.ndcg(gain))
    case class AveragePrecision[A: RelevanceJudgements]() extends MetricOp[A, Double](_.averagePrecision)
    case class ReciprocalRank[A: RelevanceJudgements]()   extends MetricOp[A, Double](_.reciprocalRank)
    case class RPrecision[A: RelevanceJudgements]()       extends MetricOp[A, Double](_.rPrecision)
    case class Precision[A: TruePositiveCount]()          extends MetricOp[A, Double](_.precision)
    case class FScore[A: RelevantCounts]()                extends MetricOp[A, Double](_.fScore)
    case class Recall[A: RelevantCounts]()                extends MetricOp[A, Double](_.recall)
  }

  case class K[A: AtK](k: Rank) extends EvalOp[A, A] { def apply(a: A): Option[A] = a.atK(k) }

  case class BinaryRelevance[A: ResultLabels](threshold: Int) extends EvalOp[A, WithGroundTruth[A]] {
    def apply(a: A): Option[WithGroundTruth[A]] =
      a.resultLabels.toNel.toList
        .mapFilter { case (id, label) => if(label >= threshold) Some(id) else None }
        .toNel.map(n => WithGroundTruth(a, n.toNes))
  }


  sealed trait EngagementOp[-A, +B] extends EvalOp[A, B]
  object EngagementOp {

    case object BinaryEngagements extends EngagementOp[EngagedResults, EngagedResults] {

      def apply(f: EngagedResults): Option[EngagedResults] =
        Some(f.copy(engagements = f.engagements.map(a => a.copy(engagements = a.engagements.map(_ => 1L)))))
    }

    sealed trait EngagementToRelevancy[-A, +B] extends EngagementOp[A, B] {
      def isRelevant(f: EngagementCounts): Boolean

      final def apply(e: EngagedResults): Option[SetRelevance] =
        e.engagements.toNel.toList.mapFilter { case (id, e) =>
          isRelevant(e).guard[Option].as(id)
        }.toNel.map(n => WithGroundTruth(e.results, n.toNes))
    }

    object EngagementToRelevancy {

      case class MoreThan(e: Engagement, n: Int) extends EngagementToRelevancy[EngagedResults, SetRelevance] {
        def isRelevant(f: EngagementCounts): Boolean = f.engagements.lookup(e).exists(_ > n)
      }

      case class Exists(e: Engagement) extends EngagementToRelevancy[EngagedResults, SetRelevance] {
        def isRelevant(f: EngagementCounts): Boolean = f.engagements.contains(e)
      }
    }

    sealed trait EngagementToLabel[-A, +B] extends EngagementOp[A, B] {
      def label(f: EngagementCounts): Option[Double]

      def apply(e: EngagedResults): Option[SetLabels] =
        e.engagements.toNel.toList
          .mapFilter { case (id, e) => label(e).map((id, _))}.toNel
          .map(n => WithLabels(e.results, n.toNem))
    }

    object EngagementToLabel {

      case class Count(e: NonEmptySet[Engagement]) extends EngagementToLabel[EngagedResults, SetLabels] {
        def label(f: EngagementCounts): Option[Label] = e.foldMap(f.engagements.lookup(_).map(_.toDouble))
      }

      // only the denominator engagment needs to be present for this to compute
      case class RatioOf(num: Engagement, den: Engagement) extends EngagementToLabel[EngagedResults, SetLabels] {
        def label(f: EngagementCounts): Option[Label] = f.engagements.lookup(den).map { p => f.engagements.lookup(num).map(_.toLong).getOrElse(0L) / p.toDouble }
      }

      case class WeightedCount(weights: NonEmptyMap[Engagement, Double]) extends EngagementToLabel[EngagedResults, SetLabels] {
        def label(f: EngagementCounts): Option[Label] = weights.toNel.foldMap { case (e, w) => f.engagements.lookup(e).map(_.toDouble * w) }
      }
    }
  }



  object compile {

    val toEvalOpLedger: EvalOp ~~> EvalOpLedger = new (EvalOp ~~> EvalOpLedger) {
      def apply[A, B](fab: EvalOp[A, B]): EvalOpLedger[A, B] = Kleisli(fab(_).foldMapK(b => WriterT.put(b)(List(fab))))
    }

    val toOption: EvalOp ~~> Kleisli[Option, *, *] = new (EvalOp ~~> Kleisli[Option, *, *]) {
      def apply[A, B](fab: EvalOp[A, B]): Kleisli[Option, A, B] = Kleisli(fab(_))
    }
  }
}