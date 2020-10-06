package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}

@typeclass trait RelevantCount[-A] extends ResultsCount[A] {

  def nRelevantResults(a: A): Int

  def nRelevant(a: A): Int

  @op("precision")
  def precision(a: A): Option[Double] =
    calc(nRelevantResults(a), nResults(a))

  @op("recall")
  def recall(a: A): Option[Double] =
    calc(nRelevantResults(a), nRelevant(a))
}
