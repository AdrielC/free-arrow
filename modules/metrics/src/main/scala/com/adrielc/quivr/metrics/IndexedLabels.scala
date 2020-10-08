package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.LabelledIndexes
import simulacrum.{op, typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.{Discount, Gain}

@typeclass trait IndexedLabels[A] extends RelevantCount[A] {

  @op("labels")
  def labels(a: A): LabelledIndexes

  @op("ndcg")
  def ndcg(a: A, g: Gain = Gain.Pow2, d: Discount = Discount.Log2p1): Option[Double] =
    labels(a).ndcg(g, d)

  @op("dcg")
  def dcg(a: A, g: Gain = Gain.Pow2, d: Discount = Discount.Log2p1): Option[Double] =
    labels(a).dcg(g, d)

  @op("averagePrecision")
  def averagePrecision(a: A): Option[Double] = {
    val l = labels(a)
    val (correct, sum) = (1 to l.k).foldLeft((0, 0.0)) { case ((c, s), k) =>
      if (l.labels.lookup(k).exists(_ > 0.0)) {
        val correct = c + 1
        val sum     = s + (correct / k.toDouble)
        (correct, sum)
      }
      else (c, s)
    }
    safeDiv(sum.toDouble, correct.toDouble)
  }

  @op("reciprocalRank")
  def reciprocalRank(a: A): Option[Double] =
    labels(a).labels.toNel
      .find(_._2 > 0.0)
      .map { case (idx, _) => 1 / idx.toDouble }

  override def nRelevant(a: A): Int =
    labels(a).labels.toNel.count { case (_, label) => label > 0.0 }.toInt

  override def nResults(a: A): Int =
    labels(a).k

  override def nRelevantResults(a: A): Int = {
    val LabelledIndexes(l, k, _) = labels(a)
    l.toNel.count { case (idx, label) => label > 0.0 && idx <= k }.toInt
  }
}
