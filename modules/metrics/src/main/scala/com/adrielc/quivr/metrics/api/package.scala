package com.adrielc.quivr.metrics

import cats.data.NonEmptyMap
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import dsl.{+>, EvalResult, eval}


package object api {
  type Metric = ResultRels +> Double
  type EngCounts[E] = Map[ResultId, Map[E, Int]]
  type Results = Seq[ResultId]
  type EvalFunction[E] = (Results, EngCounts[E]) => NonEmptyMap[String, EvalResult[Double]]

  object rel {
    def count[E](e: E): Labeler[E] = dsl.label.count(e)
    def binary[E](e: E): Judge[E] = dsl.judge.any(e)
  }

  object metric {
    val ndcg          : Metric = eval.ndcg
    val qMeasure      : Metric = eval.qMeasure()
    val f1            : Metric = eval.fScore
    val recall        : Metric = eval.recall
    val precision     : Metric = eval.precision
    val avgPrecision  : Metric = eval.averagePrecision
    val reciprocalRank: Metric = eval.reciprocalRank
    val rPrecision    : Metric = eval.rPrecision
  }
}
