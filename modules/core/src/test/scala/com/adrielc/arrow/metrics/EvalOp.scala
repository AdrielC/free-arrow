package com.adrielc.arrow.metrics

import scala.math.{log, pow}
import cats.implicits._
import com.adrielc.arrow.metrics.evaluable.{LabelledIndexes, ResultsWithRelevant}

sealed trait EvalOp[-A, +B]
object EvalOp {

  sealed trait Metric[-I] extends EvalOp[I, Double] with (I => Double)

  object Metric {

    case object Ndcg extends Metric[LabelledIndexes] {

      def apply(results: LabelledIndexes): Double = {
        val dcg = Dcg(results)
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

      def apply(indexes: LabelledIndexes): Double =
        indexes.indexedLabels.foldMap { case (i, rel) => (pow(2, rel) - 1) / (log(i + 2.0) / log(2)) }
    }

    case object Idcg extends Metric[LabelledIndexes] {

      def apply(indexes: LabelledIndexes): Double =
        Dcg(LabelledIndexes(indexes.indexedLabels.sortBy(-_._2).zipWithIndex.map { case ((_, l), i) => i -> l }))
    }
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, Option[A]] {
    def apply(a: A): Option[A] = a.toK(k)
  }
  object AtK {
    type KFiltered[A] = Either[InsufficientSize, A]
    case class InsufficientSize(minRequired: Int, maxPossible: Int)
  }
}