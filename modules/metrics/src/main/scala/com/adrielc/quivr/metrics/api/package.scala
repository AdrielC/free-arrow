package com.adrielc.quivr.metrics

import cats.data.{Kleisli, NonEmptyList, NonEmptyVector}
import com.adrielc.quivr.free.FA
import com.adrielc.quivr.metrics.data.Rankings.RankedResults
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp._
import com.adrielc.quivr.metrics.function.{discount, gain}
import cats.implicits._
import eu.timepit.refined.types.all.PosInt
import shapeless.ops.tuple.LeftReducer
import shapeless.syntax.std.tuple._
import shapeless.Poly2


package object api {
  import dsl._
  type Res[E] = (NonEmptyVector[Long], Map[Long, Map[E, Int]])
  type Metric = MetricOp[RankedResults[Relevance]]

  object label {

    def count[E](e: E): Labeler[E] = Labeler.countOf(e)

    def any[E](e: E): Judge[E] = count(e) > 0
  }
  
  object metric {
    val ndcg          : Metric = Ndcg(gain.pow2, discount.log2)
    val qMeasure      : Metric = QMeasure(1)
    val f1            : Metric = FScore()
    val recall        : Metric = Recall()
    val precision     : Metric = Precision()
    val avgPrecision  : Metric = AveragePrecision()
    val reciprocalRank: Metric = ReciprocalRank()
    val rPrecision    : Metric = RPrecision()
  }

  object evaluator {

    def apply[H <: Product, E](
      labelers  : H,
      atKs      : Seq[PosInt],
      metric    : Metric,
      metrics   : Metric*
    )(implicit M: LeftReducer.Aux[H, labelBuilder.type, Res[E] +> ResultRels]): (Seq[ResultId], Map[ResultId, Map[E, Int]]) => Map[String, Double] = {
      val in = FA.lift((se: (Seq[Long], Map[ResultId, Map[E, Int]])) => NonEmptyVector.fromVector(se._1.toVector).toRight(()).map(_ -> se._2))
      val e = labelers.reduceLeft(labelBuilder)
      val k = atKs.toList.map(atK[ResultRels](_): ResultRels +> ResultRels).toNel.map(_.reduceK).getOrElse(FA.id[ResultRels])
      val m = FA.plus(NonEmptyList(metric, metrics.toList).map(e => eval(FA.liftK(e))))

      val arrow = in >>> (e >>> k >>> m).right

      val evaluator = Kleisli(interpreter.evaluation.compileManyMetrics(arrow, interpreter.key.defaultKeyBuilder).rmap(_.toSortedMap))
      val orEmpty = evaluator.run.rmap(_.toList.mapFilter(a => a._2.toOption.flatMap(_.toOption).tupleLeft(a._1)).toMap)
      (a, b) => orEmpty((a, b))
    }
  }

  private[api] object labelBuilder extends Poly2 {
    implicit def LabelerLabel[A] = at[Labeler[A], Labeler[A]]((a, b) => a.from[Res[A]] <+> b.from[Res[A]])
    implicit def JudgeLabel[A] = at[Judge[A], Judge[A]]((a, b) => a.from[Res[A]] <+> b.from[Res[A]])
    implicit def JudgeLabelerLabel[A] = at[Judge[A], Labeler[A]]((a, b) => a.from[Res[A]] <+> b.from[Res[A]])
    implicit def LabelerJudgeLabel[A] = at[Labeler[A], Judge[A]]((a, b) => a.from[Res[A]] <+> b.from[Res[A]])
  }
}
