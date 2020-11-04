package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.Judged.WithGroundTruth
import com.adrielc.quivr.metrics.ranking.BinaryRelevance
import com.adrielc.quivr.metrics.result.{Qrels, Results}
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

    //    https://link.springer.com/article/10.1007/s10791-008-9059-7
    /**
     * Sometimes Precision is not a satisfactory metric for us because:
     * (1) It ignores the ranks of retrieved relevant documents;
     * (2) It does not average well, especially with a large document cut-off; (3) With a small document cut- off,
     *     it gives unreliable results as systems are evaluated based on a small number of observations, i.e.,
     *     documents near the top of the ranked list (Sakai 2007f).
     */
  }
  object TruePositiveCount {
    implicit def relevantResultInstance[A](implicit R: BinaryRelevance[A]): TruePositiveCount[A] = R
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

    implicit def relevanceK[A: Results : Qrels]: RelevanceCounts[A] = new RelevanceCounts[A] {
      final def groundTruthCount(a: A): Int = Qrels[A].groundTruthCount(a)
      final def resultCount(a: A): Int = Results[A].resultCount(a)
      final def truePositiveCount(a: A): Int = BinaryRelevance[A].truePositiveCount(a)
    }
  }
}
