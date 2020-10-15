package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}

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
  implicit def relevantResultInstance[A: RelevanceJudgements]: TruePositiveCount[A] = RelevanceJudgements[A]
}

@typeclass trait RelevantCounts[A] extends TruePositiveCount[A] with GroundTruthCount[A] {

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

object RelevantCounts {

  implicit def relevanceK[A: ResultSet: GroundTruthSet]: RelevantCounts[A] = GroundTruthRelevance[A]
}

