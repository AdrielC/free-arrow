package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.LabelledIndexes
import simulacrum.{op, typeclass}
import cats.implicits._

@typeclass trait IndexedLabels[A] extends RelevantCount[A] {

  @op("labels")
  def labels(a: A): LabelledIndexes

  @op("ndcg")
  def ndcg(a: A): Option[Double] =
    labels(a).ndcg

  @op("dcg")
  def dcg(a: A): Double =
    labels(a).dcg

  override def nRelevant(a: A): Int =
    labels(a).labels.toNel.count { case (_, label) => label > 0.0 }.toInt

  override def nResults(a: A): Int =
    labels(a).k

  override def nRelevantResults(a: A): Int = {
    val LabelledIndexes(l, k) = labels(a)
    l.toNel.count { case (idx, label) => label > 0.0 && idx <= k }.toInt
  }
}
