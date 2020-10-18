package com.adrielc.quivr

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}
import com.adrielc.quivr.metrics.data.Engagement
import com.adrielc.quivr.metrics.ranking.{RelevanceJudgements, RelevanceLabels}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, GroundTruthSet, ResultLabels, Results}

import scala.math.{log, pow}

package object metrics extends AtK.ToAtKOps
  with Results.ToResultsOps
  with Engagements.ToEngagementsOps
  with TruePositiveCount.ToTruePositiveCountOps
  with RelevanceJudgements.ToRelevanceJudgementsOps
  with GroundTruthSet.ToGroundTruthSetOps
  with RelevanceCounts.ToRelevanceCountsOps
  with RelevanceLabels.ToRelevanceLabelsOps
  with ResultLabels.ToResultLabelsOps
  with Engagement.EngCountSyntax {

  private[metrics] val log2 = (d: Double) => log(d) / log(2.0)
  private[metrics] val log2p1 = (i: Int) => log2(i + 1.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val calc = (a: Double, b: Int) => safeDiv(a, b.toDouble)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => if(b == 0) None else Some(a / b)
  private[metrics] def calcDcg(labels: NonEmptyList[Double], gain: function.Gain = function.Gain.Pow2): Double =
    labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (gain(label) / log2p1(idx)), idx + 1) }._1
}
