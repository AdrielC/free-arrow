package com.adrielc.quivr.metrics

import cats.arrow.Arrow
import cats.implicits._
import com.adrielc.quivr.free.FreeArrow
import com.adrielc.quivr.metrics.evaluable.{LabelledIndexes, ResultsWithLabels, ResultsWithRelevant}

import scala.math.log

sealed trait EvalOp[-A, +B] extends (A => B)
object EvalOp {

  case object IdealRanking extends EvalOp[LabelledIndexes, LabelledIndexes] {
    def apply(v1: LabelledIndexes): LabelledIndexes =
      LabelledIndexes(v1.indexedLabels.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l })
  }

  trait RelevanceJudgement extends EvalOp[Label, Boolean]

  case object IsPositive extends RelevanceJudgement {
    def apply(v1: Label): Boolean = v1 > 0
  }
  case class IsGreaterThan(threshold: Label) extends RelevanceJudgement {
    def apply(v1: Label): Boolean = v1 > threshold
  }

  case class LabelToRelevance(f: RelevanceJudgement) extends EvalOp[ResultsWithLabels, Option[ResultsWithRelevant]] {
    def apply(v1: ResultsWithLabels): Option[ResultsWithRelevant] =
      v1.labels.toSortedMap.toList
        .mapFilter { case (r, l: Label) => f(l).guard[Option].as(r) }
        .toNel
        .map(l => ResultsWithRelevant(v1.results, l.toNes))
  }

  sealed trait Metric[-I] extends EvalOp[I, Double] with (I => Double)

  object Metric {

    case object Ndcg extends Metric[LabelledIndexes] {

      def apply(results: LabelledIndexes): Double = {
        val dcg = Dcg(results)
        val idcg = IdealRanking.andThen(Dcg)(results)
        if (idcg > 0) dcg / idcg
        else 0.0
      }
    }

    case object Recall extends Metric[ResultsWithRelevant] {

      def apply(r: ResultsWithRelevant): Double =
        r.nRelevantResults / r.relevant.size.toDouble
    }

    case object Precision extends Metric[ResultsWithRelevant] {

      def apply(r: ResultsWithRelevant): Double =
        r.nRelevantResults / r.results.size.toDouble
    }

    case object Dcg extends Metric[LabelledIndexes] {

      def apply(indexes: LabelledIndexes): Double = indexes.indexedLabels.toList.foldMap { case (i, rel) => rel / log2(i + 1.0) }
      private val log2 = (i: Double) => log(i) / log(2)
    }
  }

  case class K(k: Int) {
    override def toString: String = s"@$k"
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, Option[A]] {

    def apply(a: A): Option[A] = a.filterToK(k)
  }

  case class InsufficientSize(minRequired: Int, maxPossible: Int)

  object free {
    import Metric._
    import com.adrielc.quivr.free.FreeArrow.liftK

    val ndcg                : Evaluator[LabelledIndexes, Double]        = liftK(Ndcg)
    val recall              : Evaluator[ResultsWithRelevant, Double]    = liftK(Recall)
    val precision           : Evaluator[ResultsWithRelevant, Double]    = liftK(Precision)
    def atK[A: ToK](k: Int) : Evaluator[A, Either[InsufficientSize, A]] = liftK(AtK(k)) *>^ ((a, i) => a.toRight(InsufficientSize(k, i.maxK)))

    implicit class EvalOps[A, B](private val freeEval: FreeArrow[Arrow, EvalOp, A, B]) {
      private lazy val f = freeEval.fold[Function1]
      def apply(a: A): B = f(a)
    }
  }
}