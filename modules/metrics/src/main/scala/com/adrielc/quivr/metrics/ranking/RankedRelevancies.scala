package com.adrielc.quivr.metrics
package ranking

import com.adrielc.quivr.metrics.data.Rankings.Ranked
import com.adrielc.quivr.metrics.function._
import com.adrielc.quivr.metrics.result.{BinaryRelevancy, Relevancy}
import com.adrielc.quivr.metrics.retrieval.TruePositiveCount
import simulacrum.{op, typeclass}
import eu.timepit.refined.auto._

/**
 * Represents a possibly discontiguous ranked list of relevance labels
 *
 * @tparam A
 */
@typeclass trait RankedRelevancies[A] extends Serializable {
  import result.AtK.ops._

  type Rel

  implicit def relevancy: Relevancy[Rel]

  def rankedRelevancies(a: A): Ranked[Rel]

  def ndcg(a: A, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
    calcNdcgK(gains(a), g, d)

  def ndcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
    gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

  def qMeasure(a: A, b: Double = 1): Option[Double] =
    calcQ(gains(a), b)

  def qMeasureK(a: A, k: Rank, b: Double = 1): Option[Double] =
    gains(a).atK(k).flatMap(calcQ(_, b))

  private def gains(a: A): Ranked[Double] =
    rankedRelevancies(a).map(relevancy.gain)
}

object RankedRelevancies {
  type Aux[A, R] = RankedRelevancies[A] {type Rel = R}

  implicit def fromRelevancies[A: ResultRelevancies]: RankedRelevancies[A] = ResultRelevancies[A]
}

@typeclass trait RankedJudgements[A] extends RankedRelevancies[A] with TruePositiveCount[A] {
  import result.AtK.ops._

  type Rel

  override implicit def relevancy: BinaryRelevancy[Rel]

  override def rankedRelevancies(a: A): Ranked[Rel]

  @op("avgPrec", alias = true)
  def averagePrecision(a: A): Option[Double] =
    calcAP(rankedRelevancies(a))

  @op("avgPrecK", alias = true)
  def averagePrecisionK(a: A, k: Rank): Option[Double] =
    rankedRelevancies(a).atK(k).flatMap(a => calcAP(a))

  def reciprocalRank(a: A): Option[Double] =
    calcReciprocalRank(rankedRelevancies(a).rankings)

  def rankedJudgements(a: A): Ranked[Boolean] =
    rankedRelevancies(a).map(relevancy.isRel)
}

object RankedJudgements {
  type Aux[A, R] = RankedJudgements[A] {type Rel = R}

  implicit def fromRelevancies[A: ResultJudgements]: RankedJudgements[A] = ResultJudgements[A]
}