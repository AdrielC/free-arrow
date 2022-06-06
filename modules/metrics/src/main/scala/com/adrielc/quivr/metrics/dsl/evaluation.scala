package com.adrielc.quivr
package metrics
package dsl

import cats.Order
import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.data.EngagedResults
import com.adrielc.quivr.metrics.data.Labeled.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.function.{DiscountFn, GainFn}
import com.adrielc.quivr.metrics.ranking.{RankedRelevancies, ResultRelevancies}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, ResultLabels, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCount, TruePositiveCount}
import eu.timepit.refined.types.all.PosInt

object evaluation {

  sealed trait EvalOp[A, B]
  object EvalOp {

    sealed trait EngagementOp[A, B] extends EvalOp[A, B]

    final case class ToEngagedResults[A, E]()(implicit val E: Engagements[A, E], val R: Results[A], val O: Order[E])
      extends EngagementOp[A, EngagedResults[E]]

    final case class LabelsToJudgement[A](equiv: function.Eq[Double], d: Double)(implicit val R: ResultLabels[A])
      extends EngagementOp[A, WithGroundTruth[A]]

    final case class EngagementToJudgement[A, E](e: Judge[E])(implicit val E: Engagements[A, E])
      extends EngagementOp[A, WithGroundTruth[A]]

    final case class EngagementToLabel[A, E](e: Labeler[E])(implicit val E: Engagements[A, E])
      extends EngagementOp[A, WithLabels[A]]

    final case class K[A](k: PosInt)(implicit val A: AtK[A]) extends EvalOp[A, A]

    sealed trait MetricOp[A] extends EvalOp[A, Double]
    object MetricOp {
      final case class Ndcg[A](g: GainFn, d: DiscountFn)(implicit val R: RankedRelevancies[A]) extends MetricOp[A]
      final case class QMeasure[A](b: Double)           (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
      final case class AveragePrecision[A]()            (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
      final case class ReciprocalRank[A]()              (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
      final case class RPrecision[A]()                  (implicit val R: ResultRelevancies[A]) extends MetricOp[A]
      final case class FScore[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A]
      final case class Recall[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A]
      final case class Precision[A]()                   (implicit val T: TruePositiveCount[A]) extends MetricOp[A]
    }
  }

  sealed trait EvalError extends Product with Serializable
  object EvalError {
    case class MissingError(errors: NonEmptyList[EngagedResults.EmptyError]) extends EvalError
    case class KGreaterThanMax(K: Int)   extends EvalError
    case object NoValidJudgements extends EvalError
    case object NoValidLabels     extends EvalError
    case object NoRelevant        extends EvalError
  }
}
