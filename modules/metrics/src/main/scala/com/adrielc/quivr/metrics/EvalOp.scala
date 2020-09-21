package com.adrielc.quivr.metrics

import cats.implicits._
import com.adrielc.quivr.data.Pure
import com.adrielc.quivr.free.{ACP, AR, FreeArrow}
import com.adrielc.quivr.metrics.evaluable.{LabelledIndexes, Results, ResultsWithRelevant, WithRelevant}

sealed trait EvalOp[-A, +B] {
  def apply(a: A): B
}
object EvalOp {

  trait RelevanceJudgement extends EvalOp[Label, Boolean]

  case object IsPositive extends RelevanceJudgement {
    def apply(v1: Label): Boolean = v1 > 0
  }
  case class IsGreaterThan(threshold: Label) extends RelevanceJudgement {
    def apply(v1: Label): Boolean = v1 > threshold
  }

  case class LabelToRelevance(f: RelevanceJudgement) extends EvalOp[Results[Label], Option[ResultsWithRelevant]] {
    def apply(v1: Results[Label]): Option[ResultsWithRelevant] =
      v1.results.toList
        .mapFilter { case (r, l) => l.filter(f(_)).as(r) }.toNel
        .map(l => WithRelevant(v1.results.map(_._1), l.toNes))
  }

  sealed trait Metric[-I] extends EvalOp[I, Double]

  object Metric {

    case object Ndcg extends Metric[LabelledIndexes] {

      def apply(results: LabelledIndexes): Double = {
        val dcg = Dcg(results)
        val idcg = Dcg(results.idealRanking)
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

      def apply(indexes: LabelledIndexes): Double =
        indexes.indexedLabels.toNel.foldMap { case (i, rel) => if(i <= indexes.k)  pow2(rel) / log2(i + 1.0) else 0.0 }
    }
  }

  case class K(k: Int) {
    override def toString: String = s"@$k"
  }

  case class AtK[A: ToK](k: Int) extends EvalOp[A, Option[A]] {

    def apply(a: A): Option[A] = a.filterToK(k)
  }



  case class WithLabeler[A, B](l: Labeler[A, B]) extends EvalOp[A, B] {
    def apply(v1: A): B = l.foldMap(new Pure[LabelOp]{
      def apply[C, D](fab: LabelOp[C, D]): C => D = fab(_)
    }).apply(v1)
  }


  object free {
    import Metric._
    import com.adrielc.quivr.free.FreeArrow.liftK

    val ndcg                : Evaluator[LabelledIndexes, Double]        = liftK(Ndcg)
    val recall              : Evaluator[ResultsWithRelevant, Double]    = liftK(Recall)
    val precision           : Evaluator[ResultsWithRelevant, Double]    = liftK(Precision)
    def atK[A: ToK](k: Int) : Evaluator[A, Either[InsufficientSize, A]] = liftK(AtK(k)) >*^ ((a, i) => a.toRight(InsufficientSize(k, i.maxK)))
    def withLabeler[A, B](l: Labeler[A, B]): Evaluator[A, B]            = liftK(WithLabeler(l))

    implicit class EvalOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val freeEval: FreeArrow[R, EvalOp, A, B]) {
      def apply(a: A)(implicit R: R[Function1]): B = freeEval.foldMap(new Pure[EvalOp] {
        def apply[C, D](fab: EvalOp[C, D]): C => D = fab(_)
      }).apply(a)
    }
    case class InsufficientSize(minRequired: Int, maxPossible: Int)
  }
}