package com.adrielc.quivr.metrics

import cats.data.NonEmptyMap
import simulacrum.{op, typeclass}

@typeclass trait PartialResultSet[-A] extends ResultsCount[A] {

  @op("indexedIds")
  def indexedIds(s: A): NonEmptyMap[Int, Long]

  @op("maxIndex")
  def maxIndex(s: A): Int =
    indexedIds(s).last._1

  override def nResults(a: A): Int =
    indexedIds(a).length
}
