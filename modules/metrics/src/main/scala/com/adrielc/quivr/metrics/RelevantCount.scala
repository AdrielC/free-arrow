package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}
import cats.implicits._

@typeclass trait RelevantCount[A] extends ResultsCount[A] {

  def nRelevantResults(a: A): Int

  def nRelevant(a: A): Int

  @op("precision")
  def precision(a: A): Option[Double] =
    safeDiv(nRelevantResults(a).toDouble, nResults(a).toDouble)

  @op("recall")
  def recall(a: A): Option[Double] =
    safeDiv(nRelevantResults(a).toDouble, nRelevant(a).toDouble)

  @op("fScore")
  def fScore(a: A): Option[Double] =
    (recall(a), precision(a)).mapN { (r, p) =>
      val plus = r + p
      val times = r * p
      if(plus == 0) None else Some(2 * (times / plus))
    }.flatten

  @op("rPrecision")
  def rPrecision(a: A)(implicit T: ToK[A]): Option[Double] =
    a.toK(nRelevant(a)).flatMap(precision)
}
