package com.adrielc.quivr
package metrics
package dsl

import java.time.Instant

import cats.data.{NonEmptyList, NonEmptyMap}
import engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.function.{DiscountFn, GainFn}
import com.adrielc.quivr.metrics.result.AtK
import eu.timepit.refined.types.all.PosInt

object evaluation {

  sealed trait EvalOp[A, B]
  object EvalOp {

    sealed trait QueryOp[A, B]
    object QueryOp {
      final case class QueryResults(system: System)                  extends QueryOp[Query, NonEmptyList[ResultId]]
      final case class QueryLabelSource(from: Instant, to: Instant)  extends QueryOp[Query, NonEmptyMap[ResultId, Label]]

      sealed trait System
      case object Control                     extends System
      final case class Variant(name: String)  extends System

      sealed trait Query
      case class Search(keyword: String)
      case class Navigation(taxonomy: String)
    }

    sealed trait EngagementOp[A, B] extends EvalOp[A, B]
    object EngagementOp {
      final case class EngagementToJudgement[E](e: Judge[E])  extends EngagementOp[EngRes[E], ResultRels]
      final case class EngagementToLabel[E](e: Labeler[E])    extends EngagementOp[EngRes[E], ResultRels]
    }
    
    final case class K[A](k: PosInt)(implicit val A: AtK[A]) extends EvalOp[A, A]

    sealed trait Metric[A] extends EvalOp[A, Double]
    object Metric {
      final case class Ndcg(g: GainFn, d: DiscountFn) extends Metric[ResultRels]
      case object RPrecision                          extends Metric[ResultRels]
      case class QMeasure(b: Double)                  extends Metric[ResultRels]
      case object AveragePrecision                    extends Metric[ResultRels]
      case object ReciprocalRank                      extends Metric[ResultRels]
      case object FScore                              extends Metric[ResultRels]
      case object Recall                              extends Metric[ResultRels]
      case object Precision                           extends Metric[ResultRels]
    }
  }

  sealed trait EvalError extends Product with Serializable
  case object NoResults         extends EvalError
  case object NoEngagements     extends EvalError
  case object KGreaterThanMax   extends EvalError
  case object NoValidJudgements extends EvalError
  case object NoValidLabels     extends EvalError
  case object NoRelevant        extends EvalError
}
