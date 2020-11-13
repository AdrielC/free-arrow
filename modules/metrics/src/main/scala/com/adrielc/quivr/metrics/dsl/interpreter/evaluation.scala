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
import com.adrielc.quivr.metrics.ResultRels
import com.adrielc.quivr.metrics.data.Rankings.RankedResults
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.data.relevance.Relevance.{BinaryRel, GradedRel}
import engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.key.SummarizeOps
import com.adrielc.quivr.{AC, ACP, BiFunctionK, ~~>}
import com.adrielc.quivr.metrics.implicits._

object evaluation {
  type EvalFn[A, B] = A => EvalResult[B]
  type EvalOpMap[A, B]  = Kleisli[AccumMap[List[EvalOp[_, _]], EvalError, *], A, B]
  type RunErr[A, B]     = Kleisli[EvalResult, A, B]

  val runEvalWithError: EvalOp ~~> EvalFn = new (EvalOp ~~> EvalFn) {
    import EvalOp.Metric._
    override def apply[A, B](fab: EvalOp[A, B]): EvalFn[A, B] = fab match {

      case eng: EvalOp.EngagementOp.EngagementToJudgement[e] =>
        val f = judgeF[e](eng.e)
        a: EngRes[e] => RankedResults(a, f).toRight(NoValidJudgements: EvalError)

      case eng: EvalOp.EngagementOp.EngagementToLabel[e] =>
        val f = labelF[e](eng.e)
        a: A => RankedResults(a, f).toRight(NoValidLabels: EvalError)

      case at: EvalOp.K[A] =>
        a: A => at.A.atK(a, at.k.value).toRight(KGreaterThanMax: EvalError)

      case op: EvalOp.Metric[A] => op match {
        case Ndcg(g, d)       => (_: ResultRels).ndcg(g, d).toRight(NoRelevant)
        case QMeasure(b)      => (_: ResultRels).qMeasure(b).toRight(NoRelevant)
        case Precision        => (_: ResultRels).precision.toRight(NoRelevant)
        case RPrecision       => (_: ResultRels).rPrecision.toRight(NoRelevant)
        case Recall           => (_: ResultRels).recall.toRight(NoRelevant)
        case FScore           => (_: ResultRels).fScore.toRight(NoRelevant)
        case AveragePrecision => (_: ResultRels).averagePrecision.toRight(NoRelevant)
        case ReciprocalRank   => (_: ResultRels).reciprocalRank.toRight(NoRelevant)
      }
    }

    private def judgeF[E](j: Judge[E]): Map[E, Int] => BinaryRel =
      engagemement.judge.judgementCompilerToRelevance(j).run.rmap(_.getOrElse(Relevance.irrelevant))

    private def labelF[E](l: Labeler[E]): Map[E, Int] => GradedRel =
      engagemement.label.labelerToRelevanceCompiler(l).run.rmap(_.getOrElse(Relevance.zero))
  }

  private val runEvalKleisli = runEvalWithError.andThen(BiFunctionK.functionToKleisli)

  private val runEvalKleisliOption = runEvalKleisli.mapK(λ[EvalResult ~> Option](_.toOption))

  def compileManyMetrics[A, B, M: Order](fab: FreeArrow[ACP, EvalOp, A, B], describe: SummarizeOps[EvalOp, M]): A => NonEmptyMap[M, EvalResult[B]] = {
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
