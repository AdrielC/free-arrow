package com.adrielc.quivr
package metrics
package dsl

import cats.data.{Kleisli, WriterT}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data._
import com.adrielc.quivr.metrics.dsl.engagement.Expr
import com.adrielc.quivr.metrics.function.Gain
import com.adrielc.quivr.metrics.ranking.{RelevanceJudgements, RelevanceLabels}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, ResultLabels}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}


object evaluation {

  sealed trait EvalOp[-A, +B] extends Product with Serializable { def apply(a: A): Option[B] }
  object EvalOp {

    sealed abstract class MetricOp[-A, +B](f: A => Option[B]) extends EvalOp[A, B] { final def apply(a: A): Option[B] = f(a) }
    object MetricOp {
      case class Ndcg[A: RelevanceLabels](gain: Gain)       extends MetricOp[A, Double](_.ndcg(gain))
      case class AveragePrecision[A: RelevanceJudgements]() extends MetricOp[A, Double](_.averagePrecision)
      case class ReciprocalRank[A: RelevanceJudgements]()   extends MetricOp[A, Double](_.reciprocalRank)
      case class RPrecision[A: RelevanceJudgements]()       extends MetricOp[A, Double](_.rPrecision)
      case class Precision[A: TruePositiveCount]()          extends MetricOp[A, Double](_.precision)
      case class FScore[A: RelevanceCounts]()                extends MetricOp[A, Double](_.fScore)
      case class Recall[A: RelevanceCounts]()                extends MetricOp[A, Double](_.recall)
    }

    case class K[A: AtK](k: Rank) extends EvalOp[A, A] { def apply(a: A): Option[A] = a.atK(k) }

    case class BinaryRelevance[A: ResultLabels](threshold: Int) extends EvalOp[A, WithGroundTruth[A]] {
      def apply(a: A): Option[WithGroundTruth[A]] =
        WithGroundTruth.fromResultLabels(a, _ >= threshold)
    }


    sealed trait EngagementOp[-A, +B] extends EvalOp[A, B]
    object EngagementOp {

      case class EngagementToJudgement[A: Engagements[*, E], E](expr: Expr[E]) extends EngagementOp[A, WithGroundTruth[A]] {
        private lazy val f = engagement.ExprF.evalToKleisli[Expr[E], E](expr).run.rmap(_.isDefined)
        final def apply(a: A): Option[WithGroundTruth[A]] = WithGroundTruth.fromLabels(a, a.engagementCounts, f)
      }

      case class EngagementToLabel[A: Engagements[*, E], E](expr: Expr[E]) extends EngagementOp[A, WithLabels[A]] {
        private lazy val f = engagement.ExprF.evalToKleisli[Expr[E], E](expr).run
        final def apply(a: A): Option[WithLabels[A]] = WithLabels.fromLabels(a, a.engagementCounts, f)
      }
    }


    object free {

      val toEvalOpLedger: EvalOp ~~> EvalOpLedger = new (EvalOp ~~> EvalOpLedger) {
        def apply[A, B](fab: EvalOp[A, B]): EvalOpLedger[A, B] = Kleisli(fab(_).foldMapK(b => WriterT.put(b)(List(fab))))
      }

      val toOption: EvalOp ~~> Kleisli[Option, *, *] = new (EvalOp ~~> Kleisli[Option, *, *]) {
        def apply[A, B](fab: EvalOp[A, B]): Kleisli[Option, A, B] = Kleisli(fab(_))
      }
    }
  }
}
