package com.adrielc.quivr.metrics
package retrieval

import simulacrum.{op, typeclass}

@typeclass trait RelevanceCount[A] extends TruePositiveCount[A] {

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

object RelevanceCount {
  import result.{Results, GroundTruth}

  implicit def relevanceK[A](implicit R: Results[A], G: GroundTruth[A]): RelevanceCount[A] = new RelevanceCount[A] {
    final def groundTruthCount(a: A): Int = G.groundTruthCount(a)
    final def resultCount(a: A): Int = R.resultCount(a)
    final def truePositiveCount(a: A): Int = R.results(a).toVector.count(G.groundTruth(a).set.contains)
  }
}