package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.Judged.WithGroundTruth
import com.adrielc.quivr.metrics.ranking.RelevanceJudgements
import com.adrielc.quivr.metrics.result.{GroundTruthSet, Results}
import simulacrum.{op, typeclass}
import cats.implicits._

object retrieval {

  @typeclass trait ResultCount[A] extends Serializable {

    // Number of results retrieved in this result set
    def resultCount(a: A): Int
  }

  @typeclass trait GroundTruthCount[A] extends Serializable {

    // Global or ground truth count of relevant results for this result set
    def groundTruthCount(a: A): Int
  }


  @typeclass trait TruePositiveCount[A] extends ResultCount[A] {

    // Number of relevant results in this result set
    def truePositiveCount(a: A): Int

    @op("precision")
    def precision(a: A): Option[Double] =
      safeDiv(truePositiveCount(a).toDouble, resultCount(a).toDouble)
  }
  object TruePositiveCount {
    implicit def relevantResultInstance[A](implicit R: RelevanceJudgements[A]): TruePositiveCount[A] = R
  }

  @typeclass trait RelevanceCounts[A] extends TruePositiveCount[A] {

    def groundTruthCount(a: A): Int

    @op("recall")
    def recall(a: A): Option[Double] =
      safeDiv(truePositiveCount(a).toDouble, groundTruthCount(a).toDouble)

    @op("fScore")
    def fScore(a: A): Option[Double] = {
      val rel = truePositiveCount(a).toDouble
      for {
        r <- safeDiv(rel, groundTruthCount(a).toDouble)
        p <- safeDiv(rel, resultCount(a).toDouble)
        plus = r + p
        if plus != 0
      } yield 2 * (r * p / plus)
    }
  }

  object RelevanceCounts {

    implicit def relWithG[A: Results]: RelevanceCounts[WithGroundTruth[A]] = new RelevanceCounts[WithGroundTruth[A]] {
      override def resultCount(a: WithGroundTruth[A]): Int = Results[A].resultCount(a.results)
      override def truePositiveCount(a: WithGroundTruth[A]): Int = a.results.results.count(a.groundTruth.contains).toInt
      override def groundTruthCount(a: WithGroundTruth[A]): Int = a.groundTruth.length
    }

    implicit def relevanceK[A: Results : GroundTruthSet]: RelevanceCounts[A] = new RelevanceCounts[A] {
      final def groundTruthCount(a: A): Int = GroundTruthSet[A].groundTruthCount(a)
      final def resultCount(a: A): Int = Results[A].resultCount(a)
      final def truePositiveCount(a: A): Int = RelevanceJudgements[A].truePositiveCount(a)
    }
  }
}
