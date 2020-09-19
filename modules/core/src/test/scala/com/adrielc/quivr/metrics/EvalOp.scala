package com.adrielc.quivr.metrics

import cats.arrow.Arrow

import scala.math.log
import cats.implicits._
import com.adrielc.quivr.free.FreeArrow
import com.adrielc.quivr.metrics.ToK.{InsufficientSize, KFiltered}
import com.adrielc.quivr.metrics.evaluable.{LabelledIndexes, ResultsWithRelevant}

sealed trait EvalOp[-A, +B] extends (A => B)
object EvalOp {

  sealed trait Metric[-I] extends EvalOp[I, Double] with (I => Double)

  object Metric {

    case object Ndcg extends Metric[LabelledIndexes] {

      def apply(results: LabelledIndexes): Double = {
        val dcg = results.filterK.fold(0.0)(Dcg(_))
        val idcg = Idcg(results)
        if (idcg > 0) dcg / idcg
        else 0.0
      }
    }

    case object Recall extends Metric[ResultsWithRelevant] {

      def apply(r: ResultsWithRelevant): Double =
        r.results.toNes.intersect(r.relevant).size / r.relevant.size.toDouble
    }

    case object Precision extends Metric[ResultsWithRelevant] {

      def apply(r: ResultsWithRelevant): Double =
        r.results.toNes.intersect(r.relevant).size / r.results.size.toDouble
    }

    case object Dcg extends Metric[LabelledIndexes] {

      def apply(indexes: LabelledIndexes): Double = indexes.indexedLabels.toList.foldMap { case (i, rel) => rel / log2(i + 1.0) }
      private val log2 = (i: Double) => log(i) / log(2)
    }

    case object Idcg extends Metric[LabelledIndexes] {

      def apply(indexes: LabelledIndexes): Double =
        Dcg(LabelledIndexes(indexes.indexedLabels.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }))
    }
  }

  case class K(k: Int) {
    override def toString: String = s"@$k"
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, KFiltered[A]] {

    def apply(a: A): KFiltered[A] = a.filterToK(k)
  }

  object free {
    import com.adrielc.quivr.free.FreeArrow.liftK
    import Metric._

    val ndcg                : FreeEval[LabelledIndexes, Double]     = liftK(Ndcg)
    val recall              : FreeEval[ResultsWithRelevant, Double] = liftK(Recall)
    val precision           : FreeEval[ResultsWithRelevant, Double] = liftK(Precision)
    def atK[A: ToK](k: Int) : FreeEval[A, Either[InsufficientSize, (K, A)]] = liftK(AtK(k))

    implicit class EvalOps[A, B](private val freeEval: FreeArrow[Arrow, EvalOp, A, B]) {
      private lazy val f = freeEval.fold[Function1]
      def apply(a: A): B = f(a)
    }
  }
}