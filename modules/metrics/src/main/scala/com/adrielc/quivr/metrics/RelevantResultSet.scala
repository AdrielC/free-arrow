package com.adrielc.quivr.metrics

import cats.data.NonEmptySet
import cats.implicits._
import com.adrielc.quivr.metrics.data.{LabelledIndexes, ResultsWithRelevant}
import simulacrum.{op, typeclass}

@typeclass trait RelevantResultSet[A] extends IndexedLabels[A] with ResultSet[A] {

  @op("relevant")
  def relevant(a: A): NonEmptySet[Long]

  @op("resultsWithRelevant")
  def resultsWithRelevant(a: A): ResultsWithRelevant =
    ResultsWithRelevant(resultIds(a), relevant(a))

  override def nRelevant(a: A): Int =
    relevant(a).length

  override def nRelevantResults(a: A): Int =
    resultIds(a).toNes.intersect(relevant(a)).size

  override def labels(a: A): LabelledIndexes = {
    val rel = relevant(a)
    val labs = labels(a)
    val unknownIdxRel = (rel diff resultIds(a).toNes).toList.toNel.map(_.mapWithIndex((_, idx) => (Int.MaxValue - idx) -> 1.0).toNem)
    val fullLabels = unknownIdxRel.fold(labs.labels)(labs.labels ++ _)
    LabelledIndexes(fullLabels, labs.k)
  }
}
