package com.adrielc.quivr.metrics
package dsl

import cats.implicits._
import cats.kernel.Order
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data._
import com.adrielc.quivr.metrics.ranking.{BinaryRelevance, GradedRelevance, PartialRelevancy}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, ResultLabels}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}

object evaluation {

  sealed trait EvalOp[A, B] extends Product with Serializable {
    def apply(a: A): EvalResult[B]
  }
  object EvalOp {

    sealed abstract class MetricOp[A, B](f: A => Option[B], e: EvalErr) extends EvalOp[A, B] { final def apply(a: A): EvalResult[B] = f(a).toRight(e) }
    object MetricOp {
      case class Ndcg[A: GradedRelevance](g: Gain, d: Discount) extends MetricOp[A, Double](_.ndcg(g, d),       ZeroIDCG)
      case class QMeasure[A: PartialRelevancy](b: Double)       extends MetricOp[A, Double](_.qMeasure(b),      ZeroResults)
      case class AveragePrecision[A: PartialRelevancy]()        extends MetricOp[A, Double](_.averagePrecision, ZeroResults)
      case class ReciprocalRank[A: PartialRelevancy]()          extends MetricOp[A, Double](_.reciprocalRank,   NoResults)
      case class RPrecision[A: BinaryRelevance]()               extends MetricOp[A, Double](_.rPrecision,       NoResults)
      case class FScore[A: RelevanceCounts]()                   extends MetricOp[A, Double](_.fScore,           NoResults)
      case class Recall[A: RelevanceCounts]()                   extends MetricOp[A, Double](_.recall,           NoResults)
      case class Precision[A: TruePositiveCount]()              extends MetricOp[A, Double](_.precision,        ZeroResults)
    }

    case class K[A: AtK](k: Rank) extends EvalOp[A, A] { def apply(a: A): EvalResult[A] = a.atK(k).toRight(KGreaterThanMax) }

    case class BinaryRels[A: ResultLabels](threshold: Int) extends EvalOp[A, WithGroundTruth[A]] {

      def apply(a: A): EvalResult[WithGroundTruth[A]] =
        WithGroundTruth.fromResultLabels(a, _ >= threshold).toRight(NoLabelsGtEqRelevanceThreshold)
    }


    sealed trait EngagementOp[A, B] extends EvalOp[A, B]
    object EngagementOp {

      case class EngagementToJudgement[A: Engagements[*, E], E](e: engagement.Judge[E]) extends EngagementOp[A, WithGroundTruth[A]] {

        final def apply(a: A): EvalResult[WithGroundTruth[A]] =
          WithGroundTruth.fromLabels(a, a.engagementCounts, f).toRight(NoValidEngagements)

        private lazy val f = interpreter.engagemement.judgeCompiler(e).run.rmap(_.getOrElse(false))
      }

      case class EngagementToLabel[A: Engagements[*, E], E](e: engagement.Labeler[E]) extends EngagementOp[A, WithLabels[A]] {

        final def apply(a: A): EvalResult[WithLabels[A]] =
          WithLabels.fromLabels(a, a.engagementCounts, f).toRight(NoValidEngagements)

        private lazy val f = interpreter.engagemement.labelerCompiler(e).run
      }
    }



    sealed trait EvalErr extends Product with Serializable
    case object KGreaterThanMax                 extends EvalErr
    case object NoValidEngagements              extends EvalErr
    case object NoLabelsGtEqRelevanceThreshold  extends EvalErr
    case object ZeroIDCG                        extends EvalErr
    case object ZeroResults                     extends EvalErr
    case object ZeroRelevant                    extends EvalErr
    case object NoEngagements                   extends EvalErr
    case object NoResults                       extends EvalErr

    implicit def evalOpOrder: Order[EvalOp[_, _]] = Order.by(_.hashCode())
  }
}
