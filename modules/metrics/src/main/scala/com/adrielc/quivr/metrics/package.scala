package com.adrielc.quivr

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.data.{Label, Rank, Ranked}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import cats.implicits._
import com.adrielc.quivr.metrics.ranking.{PartialRelevancies, Relevancies}
import com.adrielc.quivr.metrics.relevancy.Relevancy
import com.adrielc.quivr.metrics.result.{AtK, Engagements, GroundTruth, ResultLabels, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}

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
  with PartialRelevancies.ToPartialRelevanciesOps
  with Results.ToResultsOps
  with Engagements.ToEngagementsOps
  with TruePositiveCount.ToTruePositiveCountOps
  with Relevancies.ToRelevanciesOps
  with GroundTruth.ToGroundTruthOps
  with Relevancy.ToRelevancyOps
  with RelevanceCounts.ToRelevanceCountsOps
  with ResultLabels.ToResultLabelsOps {
  import function._

  /**
   * IDCG is only calculated to the indexes available in `labels`.
   *
   * This allows for the omission of results with 0.0 labels, since NDCG with or without them is equivalent
   *
   * Also possible to use
   *
    */
  def calcNdcgK(labels: Ranked[Label], g: GainFn, d: DiscountFn): Option[Double] = {
    val ideal = labels.copy(indexes = labels.indexes.toNel.sortBy(-_._2.value).mapWithIndex { case ((k, l), i) =>
      val r = Rank.fromIndex(i)
      val ll = if(k > labels.k) Label(0.0) else l // disregard gain from results above rank K
      r -> ll
    }.toNem)
    safeDiv(
      calcDcgK(labels, g, d),
      calcDcgK(ideal, g, d)
    )
  }
  def calcDcgK(ranked: Ranked[Label], g: GainFn, d: DiscountFn): Double =
    ranked.indexes.toNel.foldMap { case (r, label) => if(r > ranked.k) 0.0 else g(label.value) / d(r) }

  def calcDcg(labels: NonEmptyList[Label], g: GainFn, d: DiscountFn): Double =
    labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (g(label.value) / d(idx)), idx + 1) }._1

  def calcNdcg(labels: NonEmptyList[Label], g: GainFn, d: DiscountFn): Option[Double] = {
    val ideal = labels.sortBy(-_.value)
    safeDiv(
      calcDcg(labels, g, d),
      calcDcg(ideal, g, d)
    )
  }

  private[metrics] val log2 = (d: Int) => log(d + 1.0) / log(2.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => if(b == 0) None else Some(a / b)
}
