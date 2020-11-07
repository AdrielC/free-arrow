package com.adrielc.quivr

import cats.data.{NonEmptyMap, NonEmptyVector}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import cats.implicits._
import com.adrielc.quivr.metrics.data.Rankings.{Ranked, RankedResults}
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.ranking.Relevancy
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.types.numeric.{NonNegInt, PosInt}

import scala.math.{log, pow}

/**
 *
 * The traditional approach to IR evaluation is to calculate precision values based on binary relevance
 * assessments. Precision may be measured at different cut-off points of the result set or over standard recall
 * points. The former is a user-oriented and the latter system-oriented way to consider performance. Precision
 * combined with binary relevance is an easy choice for an evaluator: it is established and straightforward.
 * However, it does not take into account that degree of relevance may vary in documents.
 *
 * Measures based on the idea of cumulating gain offer a user-oriented perspective in laboratory framework.
 * The controllability is based on the use of recall bases, that is, calculation of cumulated gain is based
 * on known relevant documents. These measures support the idea of graded relevance, and are customisable
 * to the environment to be evaluated. Yet, the evaluator may have difficulties in deciding how to categorize
 * relevance or in choosing a weighting scheme for different relevance levels, i.e. in setting the parameters.
 * Nevertheless, the effort might be worthwhile because of the gained realism.
 *
 */

package object metrics {
  import function._

  type Label = Double
  type Gain = Double

  type ResultId = Long
  type ResultRels = RankedResults[Relevance]

  type Rank = PosInt
  object Rank extends RefinedTypeOps.Numeric[PosInt, Int] {
    private[metrics] def fromIndex(c: Int): Rank = PosInt.unsafeFrom(c + 1)
  }

  type Count = NonNegInt
  object Count extends RefinedTypeOps.Numeric[NonNegInt, Int]

  type NonZeroCount = PosInt
  object NonZeroCount extends RefinedTypeOps.Numeric[PosInt, Int]

  /**
   * IDCG is only calculated to the indexes available in `labels`.
   *
   * This allows for the omission of results with 0.0 labels, since NDCG with or without them is equivalent
   *
   * Also possible to use
   *
    */
  def calcNdcgK(labels: Ranked[Double], g: GainFn, d: DiscountFn): Option[Double] = {
    val ideal = labels.copy(rankings = labels.rankings.toNel.sortBy(-_._2).mapWithIndex { case ((k, l), i) =>
      val r = Rank.fromIndex(i)
      val ll = if(k > labels.k) 0.0 else l // disregard gain from results above rank K
      r -> ll
    }.toNem)
    safeDiv(
      calcDcgK(labels, g, d),
      calcDcgK(ideal, g, d)
    )
  }
  def calcDcgK(ranked: Ranked[Double], g: GainFn, d: DiscountFn): Double =
    ranked.rankings.toNel.foldMap { case (r, label) => if(r > ranked.k) 0.0 else g(label) / d(r) }

  def calcDcg(labels: NonEmptyVector[Double], g: GainFn, d: DiscountFn): Double =
    labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (g(label) / d(idx)), idx + 1) }._1

  def calcNdcg(labels: NonEmptyVector[Double], g: GainFn, d: DiscountFn): Option[Double] = {
    val ideal = labels.sortBy(-_)
    safeDiv(
      calcDcg(labels, g, d),
      calcDcg(ideal, g, d)
    )
  }

  def calcAP[R: Relevancy](rnk: Ranked[R]): Option[Double] = {
    import ranking.Relevancy.ops._
    val (correct, sum) = rnk.rankings.toNel.foldLeft((0, 0.0)) { case ((c, s), (i, r)) =>
      if (r.isRel) {
        val correct = c + 1
        val sum = s + (correct / i.toDouble)
        (correct, sum)
      } else (c, s)
    }
    safeDiv(sum.toDouble, correct.toDouble)
  }

  def calcQ[R: Relevancy](rnk: Ranked[R], b: Double): Option[Double] = {
    import Relevancy.ops._
    val labs = rnk.rankings.toNel
    val ideal = labs.sortBy(-_._2.gainOrZero)
    val withIdeal = labs.zipWith(ideal)((a, b) => (Rank.fromIndex(a._1), a._2, b._2))
    val (_, _, correct, sum) = withIdeal.foldLeft((0.0, 0.0, 0, 0.0)) {
      case (prev @ (cg, cgI, c, s), (k, r, rIdeal)) =>
        if(r.isRel) {
          val cG      = cg + r.gainOrZero
          val cGI     = cgI + rIdeal.gainOrZero
          val correct = c + 1
          val sum     = s + ((b*cG + correct) / (b*cGI + k.toDouble))
          (cG, cGI, correct, sum)
        } else prev
    }
    safeDiv(sum.toDouble, correct.toDouble)
  }

  def calcReciprocalRank[R: Relevancy](rnk: NonEmptyMap[Rank, R]): Option[Double] =
    rnk.toNel
      .find { case (_, r) => Relevancy[R].isRel(r) }
      .map { case (k, _) => 1 / k.toDouble }

  private[metrics] val log2 = (d: Int) => log(d + 1.0) / log(2.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => if(b == 0) None else Some(a / b)
}
