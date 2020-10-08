package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

/**
 * The same as [[LabelledResults]] except we don't have [[ResultId]]s
 */
case class LabelledIndexes(labels: NonEmptyMap[Index, Label], k: Int) {

  def ideal: LabelledIndexes =
    copy(labels = labels.toNel.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }.toNem)

  def dcg(gain: Label => Label): Option[Double] =
    labels.toNel.foldMap { case (i, rel) => if(i <= k) nonInf(gain(rel) * log2p1(i)) else Some(0.0) }

  def ndcg(gain: Label => Label): Option[Double] =
    (dcg(gain), ideal.dcg(gain)).mapN((d, i) => safeDiv(d, i)).flatten
}
object LabelledIndexes {

  def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes =
    new LabelledIndexes(indexedLabels.toNem, indexedLabels.toNem.last._1)

  def apply[A](results: NonEmptyList[A], relevant: NonEmptySet[A]): LabelledIndexes =
    LabelledIndexes(results.mapWithIndex((a, i) => (i + 1, if(relevant.contains(a)) 1.0 else 0.0 )))

  def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes =
    LabelledIndexes(NonEmptyList.of(h, t:_*))

  def labels(h: Label, t: Label*): LabelledIndexes =
    LabelledIndexes(NonEmptyList(h, t.toList).mapWithIndex((l, idx) => (idx + 1) -> l))

  implicit val labelledIndexesInstances: IndexedLabels[LabelledIndexes] = identity

  implicit val toKLabelledIndexes: ToK[LabelledIndexes] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))
}