package com.adrielc.quivr.metrics
package retrieval

import simulacrum.{op, typeclass}

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

  implicit def relevantResultInstance[A](implicit R: ranking.ResultJudgements[A]): TruePositiveCount[A] = R
}
