package com.adrielc.quivr.metrics

import cats.data.Kleisli
import cats.{Monoid, MonoidK, Order, Show}
import com.adrielc.quivr.free.{ACP, AR, FA, FreeArrow}
import com.adrielc.quivr.{analyze, ~>|, ~~>}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{Engagement, LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel._
import com.adrielc.quivr.metrics.dsl.EvalOp.LabelOp.Pow

package object dsl
  extends RelevantCount.ToRelevantCountOps
    with IndexedLabels.ToIndexedLabelsOps
    with ToK.ToToKOps {

  type Op           = EvalOp[_, _]
  type Eval[A, B]   = FA[EvalOp, A, B]
  type Labeler      = FA[EngagementToLabel, EngagedResults, LabelledIndexes]
  type GetLabel = EngagementCounts => Option[Double]

  val Click   : Engagement = Engagement.Click
  val CartAdd : Engagement = Engagement.CartAdd
  val Purchase: Engagement = Engagement.Purchase

  val binary    = (a: Labeler) => FreeArrow(Binary(a))
  val count     = (e: Engagement) => FreeArrow(Count(e))
  val plus      = (a: Labeler, b: Labeler) => FreeArrow(Plus(a, b))
  val percentOf = (a: Engagement, b: Engagement) => FreeArrow(PercentOf(a, b))

  def !|(w: (Engagement, Double), ws: (Engagement, Double)*)  = binary(+|(w, ws:_*))
  def +|(w: (Engagement, Double), ws: (Engagement, Double)*)  = FreeArrow(WeightedCount((w +: ws).toMap))

  val pow2: Eval[LabelledIndexes, LabelledIndexes]      = FreeArrow(Pow.Two)
  val pow1p1: Eval[LabelledIndexes, LabelledIndexes]    = FreeArrow(Pow.OnePointOne)
  val pow1p01: Eval[LabelledIndexes, LabelledIndexes]   = FreeArrow(Pow.OnePointZOne)

  object ir   extends EvalRank[ResultsWithRelevant] {
    val ndcg2  = (pow2 <<^ ((_: ResultsWithRelevant).labels)) >>> rank.ndcg
  }
  object rank extends EvalRank[LabelledIndexes] {
    val ndcg2  = pow2 >>> ndcg
  }

  implicit class EngagementOps(private val e: Engagement) extends AnyVal {
    def +(other: Engagement): Labeler = count(e) + count(other)
    def /(other: Engagement): Labeler = percentOf(e, other)
    def unary_+ : Labeler             = count(e)
    def unary_! : Labeler             = binary(count(e))
  }

  implicit class LabelerOps(private val l: Labeler) extends AnyVal {
    def +(other: Labeler): Labeler = plus(l, other)
    def +(other: Engagement): Labeler = l + count(other)
    def unary_! : Labeler = binary(l)

    private[metrics] def getLabeler: EngagementCounts => Option[Double] = l.analyze[GetLabel](new (EngagementToLabel ~>| GetLabel) {
      def apply[A, B](fab: EngagementToLabel[A, B]): GetLabel = fab.label
    })
  }

  implicit class EvalOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val e: FreeArrow[R, EvalOp, A, B]) extends AnyVal {
    def at(k: Int)(implicit T: ToK[B]): FreeArrow[R, EvalOp, A, B] = e >>> FreeArrow(EvalOp.AtK[B](k))
  }

  implicit class EngCountOps[N](private val n: N) extends AnyVal {
    def clicks(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.clicks(n)
    def click(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.clicks(n)
    def cartAdds(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.cartAdds(n)
    def cartAdd(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.cartAdds(n)
    def purchases(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.purchases(n)
    def purchase(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.purchases(n)
  }

  implicit class RichNumeric[N](n: N)(implicit N: Numeric[N]) {
    def binarize: N = if(N.gt(n, N.zero)) N.one else N.zero
  }

  def compileMetrics[R[f[_, _]] >: ACP[f] <: AR[f], F[_, _], G[_, _], A, B](
    metrics     : TraversableOnce[FreeArrow[R, F, A, B]],
    compiler    : F ~~> G,
    prefix      : String = "",
    delim       : String = ".",
    suffix      : String = ""
  )(implicit O: Order[F[_, _]], S: Show[F[_, _]], R: R[G], M: MonoidK[G[A, *]]): G[A, (String, B)] = {

    def buildMetricKey(l: List[F[_, _]]): String = l.sorted(O.toOrdering).map(S.show).foldSmash(prefix, delim, suffix)

    metrics.toList.foldMapK(_
      .summarize[List[F[_, _]]](analyze[F].list)
      .foldMap(compiler)
    ).rmap { case (k, v) => buildMetricKey(k) -> v }
  }

  val compileToList: EvalOp ~~> Kleisli[List, *, *] = new (EvalOp ~~> Kleisli[List, *, *]) {
    def apply[A, B](fab: EvalOp[A, B]): Kleisli[List, A, B] = Kleisli(fab(_).foldMapK(List(_)))
  }

  implicit val moniodLabeler: Monoid[GetLabel] =
    Monoid.instance(_ => Some(0.0), (a, b) => e => a(e) |+| b(e))

  lazy val getLabeler: EngagementToLabel ~>| GetLabel = new (EngagementToLabel ~>| GetLabel) {
    def apply[A, B](fab: EngagementToLabel[A, B]): GetLabel =
      fab match {
        case Plus(a, b) => a.analyze[GetLabel](getLabeler) |+| b.analyze[GetLabel](getLabeler)
        case other      => other.label
      }
  }
}

package dsl {

  abstract class EvalRank[A: IndexedLabels : ToK] extends Serializable {
    val atK         = (k: Int) => FreeArrow(EvalOp.AtK[A](k))
    val ndcg        = FreeArrow(EvalOp.RankingMetric.Ndcg[A])
    val recall      = FreeArrow(EvalOp.RankingMetric.Recall[A])
    val precision   = FreeArrow(EvalOp.RankingMetric.Precision[A])
  }
}
