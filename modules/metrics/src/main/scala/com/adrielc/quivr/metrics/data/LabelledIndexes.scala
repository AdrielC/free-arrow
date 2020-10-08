package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.{Discount, Gain}

/**
 * The same as [[LabelledResults]] except we don't have [[ResultId]]s
 */
case class LabelledIndexes(labels: NonEmptyMap[Index, Label], k: Int, maxK: Int) {

  def ideal: LabelledIndexes =
    copy(labels = labels.toNel.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }.toNem)

  def dcg(g: Gain = Gain.Pow2, d: Discount = Discount.Log2p1): Option[Double] =
    labels.toNel.foldMap { case (i, rel) => if(i <= k) nonInf(g(rel) * d(i)) else Some(0.0) }

  def ndcg(g: Gain = Gain.Pow2, d: Discount = Discount.Log2p1): Option[Double] =
    (dcg(g, d), ideal.dcg(g, d)).mapN((d, i) => safeDiv(d, i)).flatten
}
object LabelledIndexes {

  def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes = {
    val k = indexedLabels.toNem.last._1
    new LabelledIndexes(indexedLabels.toNem, k, k)
  }

  def apply[A](results: NonEmptyList[A], relevant: NonEmptySet[A]): LabelledIndexes =
    LabelledIndexes(results.mapWithIndex((a, i) => (i + 1, if(relevant.contains(a)) 1.0 else 0.0 )))

  def apply(r: ResultsWithRelevant): LabelledIndexes =
    LabelledIndexes(r.results.results.map(a => if(a._2) 1.0 else 0.0), r.results.k, r.results.maxK)

  def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes =
    LabelledIndexes(NonEmptyList.of(h, t:_*))

  def of(k: Int)(h: (Index, Label), t: (Index, Label)*): LabelledIndexes = {
    val nem = NonEmptyList.of(h, t:_*).toNem
    LabelledIndexes(nem, k, nem.last._1)
  }

  def labels(h: Label, t: Label*): LabelledIndexes =
    LabelledIndexes(NonEmptyList(h, t.toList).mapWithIndex((l, idx) => (idx + 1) -> l))

  implicit val labelledIndexesInstances: IndexedLabels[LabelledIndexes] = identity

  implicit val toKLabelledIndexes: ToK[LabelledIndexes] =
    (a, k) => if(k > a.maxK) None else Some(a.copy(k = k))
}