package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

case class LabelledIndexes(labels: NonEmptyMap[Index, Label], k: Int) {

  def ideal: LabelledIndexes =
    copy(labels = labels.toNel.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }.toNem)

  def dcg: Double = labels.toNel.foldMap { case (i, rel) => if(i <= k) rel / log2p1(i) else 0.0 }

  def ndcg: Option[Double] = safeDivide(dcg, ideal.dcg).toOption
}
object LabelledIndexes {

  def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes =
    new LabelledIndexes(indexedLabels.toNem, indexedLabels.toNem.last._1)

  def apply[A](results: NonEmptyList[A], relevant: NonEmptySet[A]): LabelledIndexes =
    LabelledIndexes(results.mapWithIndex((a, i) => ((i + 1), if(relevant.contains(a)) 1.0 else 0.0 )))

  def apply(r: ResultsWithRelevant): LabelledIndexes =
    LabelledIndexes(r.results, r.relevant)

  def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes =
    LabelledIndexes(NonEmptyList.of(h, t:_*).sortBy(_._1))

  def of(k: Int)(h: (Index, Label), t: (Index, Label)*): LabelledIndexes =
    LabelledIndexes(NonEmptyList.of(h, t:_*).sortBy(_._1).toNem, k)

  def labels(h: Label, t: Label*): LabelledIndexes =
    of(1 -> h, t.toList.mapWithIndex((l, i) => (i + 1) -> l):_*)

  implicit val labelledIndexesInstances: IndexedLabels[LabelledIndexes] = identity

  implicit val toKLabelledResult: ToK[LabelledIndexes] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))
}