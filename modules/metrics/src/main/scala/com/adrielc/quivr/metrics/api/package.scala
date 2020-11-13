package com.adrielc.quivr.metrics

import cats.data.Kleisli
import com.adrielc.quivr.free.FA
import cats.implicits._
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import eu.timepit.refined.types.all.PosInt
import shapeless.Lazy

package object api {
  import dsl._
  type Res[E] = (Seq[ResultId], Map[ResultId, Map[E, Int]])
  type Metric = ResultRels +> Double

  // label
  def count[E](e: E): Labeler[E] = Labeler.countOf(e)

  // judge
  def any[E](e: E): Judge[E] = count(e) > 0

  // metrics
  val ndcg          : Metric = eval.ndcg
  val qMeasure      : Metric = eval.qMeasure(1)
  val f1            : Metric = eval.fScore
  val recall        : Metric = eval.recall
  val precision     : Metric = eval.precision
  val avgPrecision  : Metric = eval.averagePrecision
  val reciprocalRank: Metric = eval.reciprocalRank
  val rPrecision    : Metric = eval.rPrecision

  object rel {

    def apply[A, E](a: A)(implicit T: Lazy[ToRel.Aux[A, E]]): Res[E] +> ResultRels =
      T.value.toRel(a)
  }


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
      val m = plus(metric, metrics:_*)
      val k = atKs.toList.map(atK[ResultRels](_): ResultRels +> ResultRels).toNel.map(_.reduceK).getOrElse(FA.id[ResultRels])
      val l = T.toRel(labelers)

      val arrow = l >>> k >>> m

      val evaluator = Kleisli(interpreter.evaluation.compileManyMetrics(arrow, interpreter.key.defaultKeyBuilder).rmap(_.toSortedMap))
      val orEmpty = evaluator.run.rmap(_.toList.mapFilter(a => a._2.toOption.tupleLeft(a._1)).toMap)
      (a, b) => orEmpty((a, b))
    }
  }
}
