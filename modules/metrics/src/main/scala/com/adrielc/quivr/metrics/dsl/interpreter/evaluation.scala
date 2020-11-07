package com.adrielc.quivr.metrics.dsl
package interpreter

import cats.data.{Kleisli, NonEmptyMap}
import com.adrielc.quivr.metrics.dsl.evaluation._
import com.adrielc.quivr.instances.all._
import com.adrielc.quivr.free.FreeArrow
import cats.implicits._
import cats.kernel.Order
import com.adrielc.quivr.data.AccumMap
import com.adrielc.quivr.metrics.data.RankedResults
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp._
import com.adrielc.quivr.metrics.dsl.key.SummarizeOps
import com.adrielc.quivr.{AC, ACP, BiFunctionK, ~~>}

object evaluation {
  type EvalFn[A, B] = A => EvalResult[B]
  type EvalOpMap[A, B]  = Kleisli[AccumMap[List[EvalOp[_, _]], EvalError, *], A, B]
  type RunErr[A, B]     = Kleisli[EvalResult, A, B]

  val runEvalWithError: EvalOp ~~> EvalFn = new (EvalOp ~~> EvalFn) {
    override def apply[A, B](fab: EvalOp[A, B]): EvalFn[A, B] = fab match {

      case res: ResultCountEq[A] => a =>
        res.eq(res.R.resultCount(a), res.k.value).guard[Option].as(a).toRight(ResultSizeFiltered(res.eq, res.k): EvalError)

      case eng: EvalOp.EngagementToJudgement[A, e] =>
        val f = judgeF[e](eng.e); implicit val e = eng.E; implicit val r = eng.R
        a: A => RankedResults[A, e, Relevance](a, f).toRight(NoValidJudgements: EvalError)

      case eng: EvalOp.EngagementToLabel[A, e] =>
        val f = labelF[e](eng.e); implicit val e = eng.E; implicit val r = eng.R
        a: A => RankedResults[A, e, Relevance](a, f).toRight(NoValidLabels: EvalError)

      case at: EvalOp.K[A] =>
        a: A => at.A.atK(a, at.k.value).toRight(KGreaterThanMax: EvalError)

      case op: EvalOp.MetricOp[A, Double] @unchecked => op match {
        case m@Ndcg(g, d)         => a => m.R.ndcg(a, g, d).toRight(NoRelevant: EvalError)
        case m@QMeasure(b)        => a => m.P.qMeasure(a, b).toRight(NoRelevant: EvalError)
        case m@Precision()        => a => m.T.precision(a).toRight(NoRelevant: EvalError)
        case m@RPrecision()       => a => m.R.rPrecision(a).toRight(NoRelevant: EvalError)
        case m@Recall()           => a => m.R.recall(a).toRight(NoRelevant: EvalError)
        case m@FScore()           => a => m.R.fScore(a).toRight(NoRelevant: EvalError)
        case m@AveragePrecision() => a => m.P.averagePrecision(a).toRight(NoRelevant: EvalError)
        case m@ReciprocalRank()   => a => m.P.reciprocalRank(a).toRight(NoRelevant: EvalError)
      }
    }

    private def judgeF[E](j: Judge[E]): Map[E, Int] => Relevance =
      engagemement.judge.judgementCompilerToRelevance(j).run.rmap(_.getOrElse(Relevance.unjudged))

    private def labelF[E](l: Labeler[E]): Map[E, Int] => Relevance =
      engagemement.label.labelerToRelevanceCompiler(l).run.rmap(_.getOrElse(Relevance.unjudged))
  }

  def compileManyMetrics[A, B, M: Order](fab: FreeArrow[ACP, EvalOp, A, B], describe: SummarizeOps[EvalOp, M]): A => NonEmptyMap[M, Either[EvalError, B]] = {
    implicit def evalOpOrder: Order[EvalOp[_, _]] = Order.by(_.hashCode())
    fab.foldMap[EvalOpMap](new (EvalOp ~~> EvalOpMap) {
      def apply[C, D](fab: EvalOp[C, D]): EvalOpMap[C, D] = {
        val f = runEvalWithError(fab)
        Kleisli(a => AccumMap.either(fab.pure[List], f(a)))
      }
    }).run.rmap(_.nem.toNel.map { case (k, v) => describe.summarize(k) -> v }.toNem)
  }

  def compileSingleMetric[A, B, M](fab: FreeArrow[AC, EvalOp, A, B], describe: SummarizeOps[EvalOp, M]): A => (M, EvalResult[B]) = {
    val descr = describe.summarize(fab.analyze(BiFunctionK.collect[EvalOp]))
    fab.foldMap[RunErr](new (EvalOp ~~> Kleisli[EvalResult, *, *]) {
      def apply[C, D](fab: EvalOp[C, D]): Kleisli[EvalResult, C, D] = {
        val f = runEvalWithError(fab)
        Kleisli(f)
      }
    }).run.rmap(descr -> _)
  }
}
