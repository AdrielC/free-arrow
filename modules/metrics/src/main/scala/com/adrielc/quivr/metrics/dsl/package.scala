package com.adrielc.quivr
package metrics

import cats.data.{Kleisli, NonEmptyList, WriterT}
import com.adrielc.quivr.free.{>>>, FA, FAP, FreeArrow}
import cats.implicits._
import FreeArrow._
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data.{EngagedResults, Rank}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.MetricOp.{AveragePrecision, FScore, Ndcg, Precision, RPrecision, Recall, ReciprocalRank}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{BinaryRelevance, EngagementOp, MetricOp}
import com.adrielc.quivr.metrics.function.Gain
import com.adrielc.quivr.metrics.dsl.serialize.MetricKeyBuilder
import com.adrielc.quivr.metrics.ranking.{RelevanceJudgements, RelevanceLabels}
import com.adrielc.quivr.metrics.result.{AtK, ResultLabels}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}

package object dsl extends engagement.ExprOps {

  type Eval[A, B]         = FA[EvalOp, A, B]
  type Evals[A, B]        = FAP[EvalOp, A, B]
  type Metric[A]          = FA[MetricOp, A, Double]
  type Engage[A, E]       = FA[EngagementOp, EngagedResults[E], A]
  type Engages[A, E]      = FAP[EngagementOp, EngagedResults[E], A]
  type Labeler[A]         = FA[EngagementOp, A, WithLabels[A]]
  type Labelers[A]        = FAP[EngagementOp, A, WithLabels[A]]
  type Judgement[A]       = FA[EngagementOp, A, WithGroundTruth[A]]
  type Judgements[A]      = FAP[EngagementOp, A, WithGroundTruth[A]]

  object judge {

    // converts labels to relevance judgements
    object label {

      def aboveThreshold[A: ResultLabels](t: Int): FA[EvalOp, A, WithGroundTruth[A]] =
        liftK(BinaryRelevance[A](t))

      def isPositive[A: ResultLabels]: FA[EvalOp, A, WithGroundTruth[A]] =
        liftK(BinaryRelevance[A](0))
    }
  }

  object atK {
    import eu.timepit.refined.cats._

    def apply[A: AtK](k: Rank): FA[EvalOp, A, A] = liftK(EvalOp.K[A](k))

    def apply[A: AtK](k: Rank, ks: Rank*): FAP[EvalOp, A, A] = FreeArrow.plus(dsl.atK[A](k), ks.map(k => dsl.atK[A](k)):_*)

    def apply[A: AtK, B](k: (Rank, Eval[A, B]), ks: (Rank, Eval[A, B])*): Evals[A, B] =
      FreeArrow.plus(NonEmptyList(k, ks.toList).groupByNem(_._1).toNel.map { case (k, n) => atK(k) >>> FreeArrow.plus(n.map(_._2)) })
  }

  object eval {
    def apply[A]: A >>> A = FreeArrow[A]
    def apply[A](m: Metric[A], ms: Metric[A]*): FAP[MetricOp, A, Double] = FreeArrow.plus(m, ms:_*)
    def ndcg[A: RelevanceLabels]: Metric[A] = ndcgWithGain(function.Gain.Pow2)
    def ndcgWithGain[A: RelevanceLabels](g: Gain): Metric[A] = ~Ndcg(g)
    def fScore[A: RelevanceCounts]: Metric[A] = ~FScore[A]
    def recall[A: RelevanceCounts]: Metric[A] = ~Recall[A]
    def precision[A: TruePositiveCount]: Metric[A] = ~Precision[A]
    def averagePrecision[A: RelevanceJudgements]: Metric[A] = ~AveragePrecision[A]
    def reciprocalRank[A: RelevanceJudgements]: Metric[A] = ~ReciprocalRank[A]
    def rPrecision[A: RelevanceJudgements]: Metric[A] = ~RPrecision[A]
    def compile[R[f[_, _]] >: ACP[f] <: AR[f], A, B](e: Eval[A, B], describeOps: MetricKeyBuilder = serialize.defaultKeys): A => Map[String, B] =
      compileToEvaluator(e, describeOps)
  }

  implicit class EvalOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val fab: FreeArrow[R, EvalOp, A, B]) extends AnyVal {

    def apply(a: A): Option[B] = fab.foldMap[Kleisli[Option, *, *]](EvalOp.free.toOption)(ArrowChoicePlus[Kleisli[Option, *, *]]).run(a)

    def compile(describeOps: MetricKeyBuilder = serialize.defaultKeys): A => Map[String, B] =
      compileToEvaluator(fab, describeOps)

    def evaluate(a: A, metricKeyBuilder: MetricKeyBuilder = serialize.defaultKeys): Map[String, B] =
      compile(metricKeyBuilder).apply(a)
  }

  type EvalOpLedger[A, B]       = OpLedger[EvalOp, A, B]
  type OpLedger[F[_, _], -A, B] = Kleisli[WriterT[List, List[F[_, _]], *], A, B]

  private[metrics] def compileToEvaluator[R[f[_, _]] >: ACP[f] <: AR[f], A, B](
    fab: FreeArrow[R, EvalOp, A, B],
    describe: MetricKeyBuilder
  ): A => Map[String, B] = {
    val evalFunction = fab.foldMap[EvalOpLedger](EvalOp.free.toEvalOpLedger)(ArrowChoicePlus[EvalOpLedger])
    evalFunction.run.rmap(_.run.map { case (k, v) => describe.summarize(k) -> v }.toMap)
  }
}