package com.adrielc.quivr.metrics

import cats.data.NonEmptySet
import cats.implicits._
import com.adrielc.quivr.metrics.data.LabelledIndexes
import simulacrum.{op, typeclass}

@typeclass trait RelevantResultSet[-A] extends IndexedLabels[A] with ResultSet[A] {

  @op("relevant")
  def relevant(a: A): NonEmptySet[Long]

  override def nRelevant(a: A): Int =
    relevant(a).length

  override def nRelevantResults(a: A): Int =
    resultIds(a).toNes.intersect(relevant(a)).size

  override def labels(a: A): LabelledIndexes = {
    val rel = relevant(a)
    val res = resultIds(a)
    val labels = res.mapWithIndex((id, idx) => (idx + 1) -> (if(rel.contains(id)) 1.0 else 0.0)).toNem
    val unknownIdxRel = (rel diff res.toNes).toList.toNel.map(_.mapWithIndex((_, idx) => (Int.MaxValue - idx) -> 1.0).toNem)
    val fullLabels = unknownIdxRel.fold(labels)(_ ++ labels)
    LabelledIndexes(fullLabels, nResults(a))
  }
}
