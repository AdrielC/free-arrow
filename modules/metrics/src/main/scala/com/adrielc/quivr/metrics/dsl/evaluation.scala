package com.adrielc.quivr.metrics
package dsl

import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}
import com.adrielc.quivr.metrics.retrieval._
import com.adrielc.quivr.metrics.ranking._
import function._

object evaluation {

  sealed trait EvalOp[A, B]
  object EvalOp {

    sealed trait FilterOp[A] extends EvalOp[A, A]
    final case class ResultCountEq[A](eq: function.Eq[Int], k: Rank)(implicit val R: ResultCount[A]) extends FilterOp[A]

    sealed trait EngagementOp[A, B] extends EvalOp[A, B]
    final case class EngagementToJudgement[A, E](e: Judge[E]) (implicit val E: Engagements[A, E], val R: Results[A]) extends EngagementOp[A, ResultRels]
    final case class EngagementToLabel[A, E](e: Labeler[E])   (implicit val E: Engagements[A, E], val R: Results[A]) extends EngagementOp[A, ResultRels]

    final case class K[A](k: Rank)(implicit val A: AtK[A]) extends EvalOp[A, A]

    sealed trait MetricOp[A, B] extends EvalOp[A, B]
    final case class Ndcg[A](g: GainFn, d: DiscountFn)(implicit val R: ResultRelevancies[A])        extends MetricOp[A, Double]
    final case class RPrecision[A]()                  (implicit val R: ResultRelevancies[A])        extends MetricOp[A, Double]
    final case class QMeasure[A](b: Double)           (implicit val P: RankedRelevancies[A]) extends MetricOp[A, Double]
    final case class AveragePrecision[A]()            (implicit val P: RankedRelevancies[A]) extends MetricOp[A, Double]
    final case class ReciprocalRank[A]()              (implicit val P: RankedRelevancies[A]) extends MetricOp[A, Double]
    final case class FScore[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A, Double]
    final case class Recall[A]()                      (implicit val R: RelevanceCount[A])    extends MetricOp[A, Double]
    final case class Precision[A]()                   (implicit val T: TruePositiveCount[A])  extends MetricOp[A, Double]
  }

  sealed trait EvalError
  case object KGreaterThanMax   extends EvalError
  case object NoValidJudgements extends EvalError
  case object NoValidLabels     extends EvalError
  case object NoRelevant        extends EvalError
  case class ResultSizeFiltered(eq: function.Eq[Int], k: Rank) extends EvalError
}
