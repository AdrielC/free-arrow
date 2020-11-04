package com.adrielc.quivr

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}
import com.adrielc.quivr.metrics.data.{Ranked, Rank}
import com.adrielc.quivr.metrics.ranking.{BinaryRelevance, GradedRelevance}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Qrels, ResultLabels, Results}
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.auto._
import cats.implicits._
import com.adrielc.quivr.metrics.relevancy.Relevancy

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

package object metrics extends AtK.ToAtKOps
  with Results.ToResultsOps
  with Engagements.ToEngagementsOps
  with TruePositiveCount.ToTruePositiveCountOps
  with BinaryRelevance.ToBinaryRelevanceOps
  with GradedRelevance.ToGradedRelevanceOps
  with Qrels.ToQrelsOps
  with Relevancy.ToRelevancyOps
  with RelevanceCounts.ToRelevanceCountsOps
  with ResultLabels.ToResultLabelsOps {


  object gain {
    val pow2    : Gain = Gain.Pow2
    val pow1p1  : Gain = Gain.Pow1p1 // avoids overflow on larger label value ranges (>1000)
    val pow1p01 : Gain = Gain.Pow1p01
    val id      : Gain = Gain.Id
  }
  object discount {

    val log2    : Discount = Discount.Log2
    val id      : Discount = Discount.Id
  }

  object double extends Eq.EqFor[Double]
  object int extends Eq.EqFor[Int]



  /**
   * IDCG is only calculated to the indexes available in `labels`.
   *
   * This allows for the omission of results with 0.0 labels, since NDCG with or without them is equivalent
   *
   * Also possible to use
   *
    */
  private[metrics] def calcNdcgK(labels: Ranked[Double], g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] = {
    val ideal = labels.copy(indexes = labels.indexes.toNel.sortBy(-_._2).mapWithIndex { case ((_, l), i) => Rank(i + 1).right.get -> l }.toNem)
    safeDiv(
      calcDcgK(labels, g, d),
      calcDcgK(ideal, g, d)
    )
  }
  private[metrics] def calcDcgK(ranked: Ranked[Double], g: Gain = gain.pow2, d: Discount = discount.log2): Double =
    ranked.indexes.toNel.foldMap { case (r, label) => if(r > ranked.k) 0.0 else g(label) / d(r) }

  private[metrics] def calcDcg(labels: NonEmptyList[Double], g: Gain = gain.pow2, d: Discount = discount.log2): Double =
    labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (g(label) / d(idx)), idx + 1) }._1

  private[metrics] def calcNdcg(labels: NonEmptyList[Double], g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] = {
    val ideal = labels.sortBy(-_)
    safeDiv(
      calcDcg(labels, g, d),
      calcDcg(ideal, g, d)
    )
  }

  private[metrics] val log2 = (d: Int) => log(d + 1.0) / log(2.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val calc = (a: Double, b: Int) => safeDiv(a, b.toDouble)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => if(b == 0) None else Some(a / b)
}
