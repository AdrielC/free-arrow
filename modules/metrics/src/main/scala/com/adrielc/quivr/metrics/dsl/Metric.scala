package com.adrielc.quivr.metrics.dsl

sealed trait Metric extends Product with Serializable
object Metric {
  case object Ndcg              extends Metric
  case object Recall            extends Metric
  case object Precision         extends Metric
  case object RPrecision        extends Metric
  case object AveragePrecision  extends Metric
  case object ReciprocalRank    extends Metric
  case object FScore            extends Metric
}