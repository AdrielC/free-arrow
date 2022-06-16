package com.adrielc.quivr.metrics

import cats.data.Kleisli
import com.adrielc.quivr.free.{FA, FAP}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Rankings.RankedResults
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.dsl.engagement.{Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import eu.timepit.refined.types.all.PosInt

package object api {
  import dsl._
  type Res[E] = (Seq[ResultId], Map[ResultId, Map[E, Int]])
  type Metric = ResultRels +> Double

  // label
  def count[E](e: E): Labeler[E] = Labeler.countOf(e)

  // judge
  def any[E](e: E) = count(e) > 0

  // metrics
  val ndcg          : Metric = eval.ndcg
  val qMeasure      : Metric = eval.qMeasure(1)
  val f1            : Metric = eval.fScore
  val recall        : Metric = eval.recall
  val precision     : Metric = eval.precision
  val avgPrecision  : Metric = eval.averagePrecision
  val reciprocalRank: Metric = eval.reciprocalRank
  val rPrecision    : Metric = eval.rPrecision


  object evaluator {

    def apply[H, E](
      labelers  : H
    )(
      metric    : Metric,
      metrics   : Metric*
    )(
      atKs      : PosInt*
    )(implicit T: ToRel.Aux[H, E]): (Seq[ResultId], Map[ResultId, Map[E, Int]]) => Map[String, Double] = {
      import FA._

      type FromRels[A] = FAP[EvalOp, ResultRels, A]

      val l = T.toRel(labelers).rmap(r =>
        RankedResults.fromEngagedResults(r.results,
          (m: Map[E, Int]) => Relevance.label((m: Map[E, Int]).values.sum.toDouble)).get
      )

      val k = atKs.toList.map(atK[ResultRels](_): FromRels[ResultRels]).toNel.map(_.reduceMapK(identity))
        .getOrElse(FA.id[ResultRels])


      val m: ResultRels +> Double = plus(metric, metrics:_*)

      val arrow = (l >>> k) >>> m
      val evaluator = Kleisli(
        interpreter.evaluation.compileManyMetrics(arrow,
        interpreter.key.defaultKeyBuilder).rmap(_.toSortedMap))
      val orEmpty = evaluator.run.rmap(_.toList.mapFilter(a => a._2.toOption.tupleLeft(a._1)).toMap)
      (a, b) => orEmpty((a, b))
    }
  }
}
