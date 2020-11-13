package com.adrielc.quivr

import cats.data.{NonEmptyMap, NonEmptyVector}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import cats.implicits._
import com.adrielc.quivr.metrics.data.Rankings.{Ranked, RankedResults}
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.function.{DiscountFn, GainFn}
import com.adrielc.quivr.metrics.result.BinaryRelevancy
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
    labels.rankings.toSortedMap.filterKeys(_ <= labels.k).values.toVector.sortBy(-_).toNev.flatMap { ideal =>
      if(labels.k == Rank.unsafeFrom(20)) {
        println("last\t\t" + labels.rankings.last)
        println("labels\t\t" + labels)
//        println(ideal)
      }
      safeDiv(
        calcDcgK(labels, g, d),
        calcDcg(ideal, g, d)
      )
    }
  }

  def calcDcgK(ranked: Ranked[Double], g: GainFn, d: DiscountFn): Double =
    ranked.rankings.toNel.foldMap { case (r, label) => if(r > ranked.k) 0.0 else g(label) / d(r) }

  def calcAP[R: BinaryRelevancy](rnk: Ranked[R]): Option[Double] = {
    val (correct, sum) = rnk.rankings.toNel.foldLeft((0, 0.0)) { case ((c, s), (k, r)) =>
      if (BinaryRelevancy[R].isRel(r)) {
        val correct = c + 1
        val sum = s + (correct / k.toDouble)
        (correct, sum)
      } else (c, s)
    }
    safeDiv(sum.toDouble, correct.toDouble)
  }

  def calcDcg(labels: NonEmptyVector[Double], g: GainFn, d: DiscountFn): Double =
    labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (g(label) / d(idx)), idx + 1) }._1

  def calcNdcg(labels: NonEmptyVector[Double], g: GainFn, d: DiscountFn): Option[Double] = {
    val ideal = labels.sortBy(-_)
    safeDiv(
      calcDcg(labels, g, d),
      calcDcg(ideal, g, d)
    )
  }

  //    Because BR(r) has an r in the denominator (just like P(r)),
  //    Q-measure is guaranteed to become smaller as a relevant document goes
  //    down the ranked list. A large b (e.g., b = 100) alleviates this effect,
  //    and makes Q-measure more forgiving for relevant documents near the bottom of the ranked list.
  //    Conversely, a small b (e.g., b = 1) imposes more penalty
  def calcQ(rnk: Ranked[Double], b: Double): Option[Double] = {
    val labs = rnk.rankings.toNel
    val ideal = labs.map { case (k,r) => k -> (if(k > rnk.k) 0.0 else r)}.sortBy(-_._2)
    val withIdeal = labs.zipWith(ideal)((a, b) => (Rank.fromIndex(a._1), a._2, b._2))
    val (_, _, correct, sum) = withIdeal.foldLeft((0.0, 0.0, 0, 0.0)) {
      case (prev @ (cg, cgI, c, s), (k, r, rIdeal)) =>
        if(r > 0) {
          val cG      = cg + r
          val cGI     = cgI + rIdeal
          val correct = c + 1
          val sum     = s + ((b*cG + correct) / (b*cGI + k.toDouble))
          (cG, cGI, correct, sum)
        } else prev
    }
    safeDiv(sum.toDouble, correct.toDouble)
  }

  def calcReciprocalRank[R: BinaryRelevancy](rnk: NonEmptyMap[Rank, R]): Option[Double] =
    rnk.toNel
      .find(r => BinaryRelevancy[R].isRel(r._2))
      .map { case (k, _) => 1 / k.toDouble }


  def calcF1(truePositiveCount: Int, groundTruthCount: Int, resultCount: Int): Option[Double] = {
    for {
      r <- safeDivInt(truePositiveCount, groundTruthCount)
      p <- safeDivInt(truePositiveCount, resultCount)
      plus = r + p
      if plus != 0
    } yield 2 * (r * p / plus)
  }


  private[metrics] val log2 = (d: Int) => log(d + 1.0) / log(2.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => if(b == 0) None else Some(a / b)
  private[metrics] val safeDivInt: (Int, Int) => Option[Double] = (a, b) => safeDiv(a.toDouble, b.toDouble)
}
