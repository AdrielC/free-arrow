package com.adrielc.quivr
package metrics
package dsl

import com.adrielc.quivr.metrics.data.Rankings.RankedResults
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.function.{DiscountFn, Eq, GainFn}
import com.adrielc.quivr.metrics.ranking.{RankedRelevancies, ResultRelevancies}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCount, TruePositiveCount}
import eu.timepit.refined.types.all.PosInt

object evaluation {

  sealed trait EvalOp[A, B]
  object EvalOp {

    sealed trait EngagementOp[A, B] extends EvalOp[A, B]

    final case class EngagementToJudgement[A, E](e: Judge[E])(implicit val E: Engagements[A, E], val R: Results[A])
      extends EngagementOp[A, RankedResults[Relevance]]

    final case class EngagementToLabel[A, E](e: Labeler[E])(implicit val E: Engagements[A, E], val R: Results[A])
      extends EngagementOp[A, RankedResults[Relevance]]

    final case class K[A](k: PosInt)(implicit val A: AtK[A]) extends EvalOp[A, A]

    sealed trait MetricOp[A] extends EvalOp[A, Double]
    final case class Ndcg[A](g: GainFn, d: DiscountFn)(implicit val R: ResultRelevancies[A]) extends MetricOp[A]
    final case class RPrecision[A]()                  (implicit val R: ResultRelevancies[A]) extends MetricOp[A]
    final case class QMeasure[A](b: Double)           (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
    final case class AveragePrecision[A]()            (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
    final case class ReciprocalRank[A]()              (implicit val P: RankedRelevancies[A]) extends MetricOp[A]
    final case class FScore[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A]
    final case class Recall[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A]
    final case class Precision[A]()                   (implicit val T: TruePositiveCount[A]) extends MetricOp[A]
  }

  sealed trait EvalError
  case object NoResults         extends EvalError
  case object NoEngagements     extends EvalError
  case object KGreaterThanMax   extends EvalError
  case object NoValidJudgements extends EvalError
  case object NoValidLabels     extends EvalError
  case object NoRelevant        extends EvalError
  case class ResultSizeFiltered(eq: Eq[Int], k: PosInt) extends EvalError
}
