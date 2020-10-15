package com.adrielc.quivr.metrics

import cats.data.{Kleisli, NonEmptyList, NonEmptyMap, NonEmptySet, WriterT}
import cats.Order
import com.adrielc.quivr.free.{ACP, AR, FA, FAP, FreeArrow}
import com.adrielc.quivr.{ArrowChoicePlus, ArrowPlus, ~>|}
import cats.implicits._
import FreeArrow.liftK
import cats.arrow.Arrow
import com.adrielc.quivr.metrics.data.Judged.WithGroundTruth
import com.adrielc.quivr.metrics.data.{SetLabels, SetRelevance}
import com.adrielc.quivr.metrics.dsl.EvalOp.{BinaryRelevance, EngagementOp, K, MetricOp}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.{BinaryEngagements, EngagementToLabel, EngagementToRelevancy}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel.{Count, RatioOf, WeightedCount}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToRelevancy.{Exists, MoreThan}
import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.{AveragePrecision, FScore, Ndcg, Precision, RPrecision, Recall, ReciprocalRank}
import com.adrielc.quivr.metrics.function.Gain
import com.adrielc.quivr.metrics.function.Gain.Pow2
import eu.timepit.refined.types.numeric.PosInt

package object dsl {

  type Eval[A, B]         = FreeArrow[Arrow, EvalOp, A, B]
  type Evals[A, B]        = FreeArrow[ArrowPlus, EvalOp, A, B]
  type Metric[A]          = FA[MetricOp, A, Double]
  type Engage[A]          = FA[EngagementOp, EngagedResults, A]
  type Labeler            = FA[EngagementToLabel, EngagedResults, SetLabels]
  type Judgement          = FA[EngagementToRelevancy, EngagedResults, SetRelevance]
  type MetricKeyBuilder   = DescribeOps[EvalOp, String]


  implicit class EngOp(val e: Engagement) extends AnyVal {

    def +(other: Engagement*): Labeler  = label.engagement.count(e, other:_*)

    def /(other: Engagement): Labeler   = label.engagement.count.ratioOf(e, other)

    def *(weight: Double): Labeler      = label.engagement.count.weighted(e -> weight)

    def count: Labeler                  = label.engagement.count(e)

    def binarize: Eval[EngagedResults, SetLabels] = engagement.binary(label.engagement.count(e))

    def exist: Judgement                = judge.engagement.exists(e)

    def >(threshold: Int): Judgement    = judge.engagement.moreThan(e, threshold)
  }

  val QuickView : Engagement = Engagement.QuickView
  val Click     : Engagement = Engagement.Click
  val Favorite  : Engagement = Engagement.Favorite
  val CartAdd   : Engagement = Engagement.CartAdd
  val Purchase  : Engagement = Engagement.Purchase
  val Review    : Engagement = Engagement.Review


  object engagement extends Together[EngagementOp] {

    // Binarizes the engagement counts, not necessarily the label
    object binary {

      def apply[B](e: Engage[B]): Engage[B] = binary.all >>> e

      def apply[B](e: Engage[B], es: Engage[B]*): FAP[EngagementOp, EngagedResults, B] = binary.all >>> engagement.together(e, es:_*)

      private val all: Engage[EngagedResults] = liftK(BinaryEngagements)
    }
  }

  object label extends Together[EngagementToLabel] {

    object engagement {

      object count {

        def apply(e: Engagement, es: Engagement*): Labeler = liftK(Count(NonEmptySet.of(e, es:_*)))

        def ratioOf(n: Engagement, d: Engagement): Labeler = liftK(RatioOf(n, d))

        def weighted(w: (Engagement, Double), ws: (Engagement, Double)*): Labeler = liftK(WeightedCount(NonEmptyMap.of(w, ws:_*)))
      }
    }
  }

  object judge {

    object engagement extends Together[EngagementToRelevancy] {

      def exists(e: Engagement): Judgement = liftK(Exists(e))

      def moreThan(e: Engagement, t: Int): Judgement = liftK(MoreThan(e, t))
    }

    // converts labels to relevance judgements
    object label {

      def aboveThreshold[A: ResultLabels](t: Int): Eval[A, WithGroundTruth[A]] = liftK(BinaryRelevance[A](t))

      def isPositive[A: ResultLabels]: Eval[A, WithGroundTruth[A]] = judge.label.aboveThreshold(0)
    }
  }

  object atK extends Together[EvalOp] {

    def apply[A: AtK](k: Int): FA[EvalOp, A, A] = liftK(EvalOp.K[A](PosInt.unsafeFrom(k)))

    def apply[A: AtK](k: Int, ks: Int*): FAP[EvalOp, A, A] = atK.together(dsl.atK[A](k), ks.map(k => dsl.atK[A](k)):_*)

    def apply[A: AtK, B](k: (Int, Eval[A, B]), ks: (Int, Eval[A, B])*): Evals[A, B] =
      FreeArrow.plusAll(NonEmptyList(k, ks.toList).groupByNem(_._1).toNel.map { case (k, n) => atK(k) >>> atK.together(n.map(_._2)) })
  }

  object eval extends Together[MetricOp] {

    object rank {

      def ndcg[A: LabelledSet]: Metric[A]                     = liftK(Ndcg(function.Gain.Pow2))
      def ndcgWithGain[A: LabelledSet](g: Gain): Metric[A]    = liftK(Ndcg(g))

      def averagePrecision[A: RelevanceJudgements]: Metric[A] = liftK(AveragePrecision[A])
      def reciprocalRank[A: RelevanceJudgements]: Metric[A]   = liftK(ReciprocalRank[A])
      def rPrecision[A: RelevanceJudgements]: Metric[A]       = liftK(RPrecision[A])
    }

    object retrieval {

      def fScore[A: RelevantCounts]: Metric[A]                = liftK(FScore[A])
      def recall[A: RelevantCounts]: Metric[A]                = liftK(Recall[A])
      def precision[A: TruePositiveCount]: Metric[A]          = liftK(Precision[A])
    }

    def compile[R[f[_, _]] >: ACP[f] <: AR[f], A, B](
      e: Eval[A, B],
      describeOps: MetricKeyBuilder = defaultMetricKeyBuilder
    ): A => Map[String, B] =
      compileToEvaluator(e, describeOps)
  }

  implicit class EvalOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val fab: FreeArrow[R, EvalOp, A, B]) extends AnyVal {

    def apply(a: A): Option[B] = fab.foldMap[Kleisli[Option, *, *]](EvalOp.compile.toOption)(ArrowChoicePlus[Kleisli[Option, *, *]]).run(a)

    def compile(describeOps: MetricKeyBuilder = defaultMetricKeyBuilder): A => Map[String, B] =
      compileToEvaluator(fab, describeOps)

    def evaluate(a: A, metricKeyBuilder: MetricKeyBuilder = defaultMetricKeyBuilder): Map[String, B] =
      compile(metricKeyBuilder).apply(a)
  }

  val defaultMetricKeyBuilder: MetricKeyBuilder = dsl.DescribeOps[EvalOp, String](
    new (EvalOp ~>| String) {
      def apply[A, B](fab: EvalOp[A, B]): String = fab match {
        case Ndcg(Pow2)             => "ndcg"
        case Ndcg(g)                => s"ndcg-$g"
        case Precision()            => "precision"
        case RPrecision()           => "r-precision"
        case Recall()               => "recall"
        case FScore()               => "f1"
        case AveragePrecision()     => "avgPrecision"
        case ReciprocalRank()       => "reciprocalRank"
        case m: MoreThan            => s"${m.e}>${m.n}"
        case h: Exists              => s"any${h.e}"
        case c: Count               => c.e.map(_.toString).foldSmash("count(", "+", ")")
        case p: RatioOf             => s"${p.num}/${p.den}"
        case w: WeightedCount       => formatWeights(w.weights).foldSmash("wtCount(", "+", ")")
        case BinaryEngagements      => s"binaryEngagement"
        case b: BinaryRelevance[_]  => s"binaryRelevance>${b.threshold}"
        case k: K[_]                => s"@${k.k}"
      }
    },
    Order.by {
      case _: EvalOp.EngagementOp[_, _] => 1
      case _: EvalOp.BinaryRelevance[_] => 2
      case _: EvalOp.MetricOp[_, _]     => 3
      case EvalOp.K(_)                  => 4
    },
    prefix = "",
    delim = ".",
    suffix = ""
  )

  type EvalOpLedger[A, B]       = OpLedger[EvalOp, A, B]
  type OpLedger[F[_, _], -A, B] = Kleisli[WriterT[List, List[F[_, _]], *], A, B]

  private[metrics] def compileToEvaluator[R[f[_, _]] >: ACP[f] <: AR[f], A, B](
    fab: FreeArrow[R, EvalOp, A, B],
    describe: MetricKeyBuilder
  ): A => Map[String, B] = {
    val evalFunction = fab.foldMap[EvalOpLedger](EvalOp.compile.toEvalOpLedger)(ArrowChoicePlus[EvalOpLedger])
    evalFunction.run.rmap(_.run.map { case (k, v) => describe.summarize(k) -> v }.toMap)
  }

  private[metrics] def formatWeights(weights: NonEmptyMap[Engagement, Double]): List[String] =
    weights.toNel.toList.map { case (eng, weight) => weight.toString.replace('.', 'p') + "x" + eng.toString }
}

package dsl {

  /** combines [[Eval]]s together while keeping individual inputs/outputs independent */
  class Together[F[_, _]] { self =>

    def together[A, B](e: FA[F, A, B], es: FA[F, A, B]*): FAP[F, A, B] = FreeArrow.plusAll(e, es:_*)

    def together[A, B](e: NonEmptyList[FA[F, A, B]]): FAP[F, A, B] = FreeArrow.plusAll(e)
  }
}