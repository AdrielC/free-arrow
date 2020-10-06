package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import simulacrum.{op, typeclass}

@typeclass trait ResultSet[-A] extends PartialResultSet[A] {

  @op("resultIds")
  def resultIds(a: A): NonEmptyList[Long]

  override def nResults(a: A): Int =
    resultIds(a).length

  override def indexedIds(a: A): NonEmptyMap[Int, Long] =
    resultIds(a).mapWithIndex((id, idx) => (idx + 1) -> id ).toNem
}
