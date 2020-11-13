package com.adrielc.quivr.metrics.dsl
package interpreter

import cats.data.{Kleisli, NonEmptyMap}
import com.adrielc.quivr.metrics.dsl.evaluation._
import com.adrielc.quivr.instances.all._
import com.adrielc.quivr.free.FreeArrow
import cats.implicits._
import cats.kernel.Order
import cats.~>
import com.adrielc.quivr.data.{AccumMap, AccumWriter}
import com.adrielc.quivr.metrics.data.EngagedResults
import com.adrielc.quivr.metrics.data.Rankings.RankedResults
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
    import EvalError._

    override def apply[A, B](fab: EvalOp[A, B]): EvalFn[A, B] = fab match {

      case toEng: ToEngagedResults[A, e] => import toEng._
        a: A => EngagedResults.fromResultsWithEngagements[A, e](a).leftMap(MissingError(_): EvalError)

      case eng: EvalOp.EngagementToJudgement[A, e] =>
        val f = judgeF[e](eng.e); import eng._
        a: A => RankedResults.fromEngagedResults[A, e](a, f).toRight(NoValidJudgements: EvalError)

      case eng: EvalOp.EngagementToLabel[A, e] =>
        val f = labelF[e](eng.e); import eng._
        a: A => RankedResults.fromEngagedResults[A, e](a, f).toRight(NoValidLabels: EvalError)

      case at: EvalOp.K[A] =>
        a: A => at.A.atK(a, at.k.value).toRight(KGreaterThanMax(at.k.value): EvalError)


      case op: EvalOp.MetricOp[A] =>
        import MetricOp._
        op match {
          case m@Ndcg(g, d)         => a => m.R.ndcg(a, g, d).toRight(NoRelevant)
          case m@QMeasure(b)        => a => m.P.qMeasure(a, b).toRight(NoRelevant)
          case m@Precision()        => a => m.T.precision(a).toRight(NoRelevant)
          case m@RPrecision()       => a => m.R.rPrecision(a).toRight(NoRelevant)
          case m@Recall()           => a => m.R.recall(a).toRight(NoRelevant)
          case m@FScore()           => a => m.R.fScore(a).toRight(NoRelevant)
          case m@AveragePrecision() => a => m.P.averagePrecision(a).toRight(NoRelevant)
          case m@ReciprocalRank()   => a => m.P.reciprocalRank(a).toRight(NoRelevant)
        }
    }

    private def judgeF[E](j: Judge[E]): Map[E, Int] => Relevance =
      engagemement.judge.judgementCompilerToRelevance(j).run.rmap(_.getOrElse(Relevance.irrelevant))

    private def labelF[E](l: Labeler[E]): Map[E, Int] => Relevance =
      engagemement.label.labelerToRelevanceCompiler(l).run.rmap(_.getOrElse(Relevance.zero))
  }

  private val runEvalKleisli = runEvalWithError.andThen(BiFunctionK.functionToKleisli)

  private val runEvalKleisliOption = runEvalKleisli.mapK(λ[EvalResult ~> Option](_.toOption))

  def compileManyMetrics[A, B, M: Order](fab: FreeArrow[ACP, EvalOp, A, B], describe: SummarizeOps[EvalOp, M]): A => NonEmptyMap[M, Either[EvalError, B]] = {
    implicit def evalOpOrder: Order[EvalOp[_, _]] = Order.by(_.hashCode())
    fab.foldMap[EvalOpMap](new (EvalOp ~~> EvalOpMap) {
      def apply[C, D](fab: EvalOp[C, D]): EvalOpMap[C, D] = {
        val f = runEvalWithError(fab)
        Kleisli(a => AccumMap.either(fab.pure[List], f(a)))
      }
    }).run.rmap(_.nem.toNel.map { case (k, v) => describe.summarize(k) -> v }.toNem)
  }

  def compileSingle[A, B, M](fab: FreeArrow[AC, EvalOp, A, B], describe: SummarizeOps[EvalOp, M]): A => (M, Option[B]) =
    fab
      .foldMap(AccumWriter.toAccumWriterKleisli(runEvalKleisliOption))
      .run.rmap(a => describe.summarize(a.l) -> a.run)
}
