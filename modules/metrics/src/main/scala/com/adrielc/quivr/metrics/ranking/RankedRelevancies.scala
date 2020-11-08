package com.adrielc.quivr.metrics
package ranking

import com.adrielc.quivr.metrics.data.Rankings.Ranked
import com.adrielc.quivr.metrics.function._
import com.adrielc.quivr.metrics.result.Relevancy
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

  implicit def rel: Relevancy[Rel]

  def rankedRelevancies(a: A): Ranked[Rel]

  def ndcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
    gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

  def dcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
    gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

  @op("avgPrec", alias = true)
  def averagePrecision(a: A): Option[Double] =
    calcAP(rankedRelevancies(a))

  @op("avgPrecK", alias = true)
  def averagePrecisionK(a: A, k: Rank): Option[Double] =
    rankedRelevancies(a).atK(k).flatMap(a => calcAP(a))

  //    Because BR(r) has an r in the denominator (just like P(r)),
  //    Q-measure is guaranteed to become smaller as a relevant document goes
  //    down the ranked list. A large b (e.g., b = 100) alleviates this effect,
  //    and makes Q-measure more forgiving for relevant documents near the bottom of the ranked list.
  //    Conversely, a small b (e.g., b = 1) imposes more penalty
  def qMeasure(a: A, b: Double = 1): Option[Double] =
    calcQ(rankedRelevancies(a), b)


  def qMeasureK(a: A, k: Rank, b: Double = 1): Option[Double] =
    gains(a).atK(k).flatMap(calcQ(_, b))

  def reciprocalRank(a: A): Option[Double] =
    calcReciprocalRank(rankedRelevancies(a).rankings)

  private def gains(a: A): Ranked[Double] =
    rankedRelevancies(a).map(rel.gainOrZero)
}

object RankedRelevancies {
  type Aux[A, R] = RankedRelevancies[A] {type Rel = R}

  implicit def fromRelevancies[A: ResultRelevancies]: RankedRelevancies[A] = ResultRelevancies[A]
}
