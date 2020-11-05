package com.adrielc.quivr.metrics
package dsl

import com.adrielc.quivr.metrics.data._
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.ranking.{PartialRelevancies, Relevancies}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}
import function._

object evaluation {

  sealed trait EvalOp[A, B]
  object EvalOp {

    sealed trait EngagementOp[A, B] extends EvalOp[A, B]
    final case class EngagementToJudgement[A, E](e: Judge[E]) (implicit val E: Engagements[A, E], val R: Results[A]) extends EngagementOp[A, ResultRels]
    final case class EngagementToLabel[A, E](e: Labeler[E])   (implicit val E: Engagements[A, E], val R: Results[A]) extends EngagementOp[A, ResultRels]

    final case class K[A](k: Rank)(implicit val A: AtK[A]) extends EvalOp[A, A]

    sealed trait MetricOp[A, B] extends EvalOp[A, B]
    final case class Ndcg[A](g: GainFn, d: DiscountFn)(implicit val R: Relevancies[A])        extends MetricOp[A, Double]
    final case class RPrecision[A]()                  (implicit val R: Relevancies[A])        extends MetricOp[A, Double]
    final case class QMeasure[A](b: Double)           (implicit val P: PartialRelevancies[A]) extends MetricOp[A, Double]
    final case class AveragePrecision[A]()            (implicit val P: PartialRelevancies[A]) extends MetricOp[A, Double]
    final case class ReciprocalRank[A]()              (implicit val P: PartialRelevancies[A]) extends MetricOp[A, Double]
    final case class FScore[A]()                      (implicit val R: RelevanceCounts[A])    extends MetricOp[A, Double]
    final case class Recall[A]()                      (implicit val R: RelevanceCounts[A])    extends MetricOp[A, Double]
    final case class Precision[A]()                   (implicit val T: TruePositiveCount[A])  extends MetricOp[A, Double]
  }

  sealed trait EvalError
  case object KGreaterThanMax   extends EvalError
  case object NoValidJudgements extends EvalError
  case object NoValidLabels     extends EvalError
  case object NoRelevant        extends EvalError
}
