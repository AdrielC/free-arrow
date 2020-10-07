package com.adrielc.quivr.metrics

import cats.data.Kleisli
import cats.{Monoid, MonoidK, Order, Show}
import com.adrielc.quivr.free.{ACP, AP, AR, FA, FreeArrow}
import com.adrielc.quivr.{ArrowPlus, analyze, ~>|, ~~>}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel._
import com.adrielc.quivr.metrics.dsl.EvalOp.LabelOp.Pow

package object dsl
  extends RelevantCount.ToRelevantCountOps
    with IndexedLabels.ToIndexedLabelsOps
    with ToK.ToToKOps {

  type Op                 = EvalOp[_, _]
  type Eval[A, B]         = FA[EvalOp, A, B]
  type Labeler            = FA[EngagementToLabel, EngagedResults, LabelledIndexes]
  type GetLabel           = EngagementCounts => Option[Double]
  type EvalOpLedger[A, B] = OpLedger[EvalOp, A, B]
  type RunMap[A, B]       = A => Map[String, B]

  type EngagementWt = Map[Engagement, Double]
  object EngagementWt {
    def apply(e: (Engagement, Double), es: (Engagement, Double)*): EngagementWt =
      (e +: es).toMap

    val oneFiveTwentyFive : EngagementWt = Map(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0)
    val onlyPurchTwenty   : EngagementWt = Map(Purchase -> 20)
    val onlyPurchOne      : EngagementWt = Map(Purchase -> 1)
    val cartOnePurchTen   : EngagementWt = Map(CartAdd -> 1, Purchase -> 10)
  }

  val Click     : Engagement = Engagement.Click
  val CartAdd   : Engagement = Engagement.CartAdd
  val Purchase  : Engagement = Engagement.Purchase
  val QuickView : Engagement = Engagement.QuickView
  val Favorite  : Engagement = Engagement.Favorite
  val Review    : Engagement = Engagement.Review

  val Ndcg              = Metric.Ndcg
  val Recall            = Metric.Recall
  val Precision         = Metric.Precision
  val RPrecision        = Metric.RPrecision
  val AveragePrecision  = Metric.AveragePrecision
  val ReciprocalRank    = Metric.ReciprocalRank
  val FScore            = Metric.FScore

  val p2                = FreeArrow(Pow.P2)
  val p11               = FreeArrow(Pow.P11)
  val p101              = FreeArrow(Pow.P101)

  val binary    = (a: Labeler) => FreeArrow(Binary(a))
  val count     = (e: Engagement) => FreeArrow(Count(e))
  val plus      = (a: Labeler, b: Labeler) => FreeArrow(Plus(a, b))
  val percentOf = (a: Engagement, b: Engagement) => FreeArrow(PercentOf(a, b))

  def !|(w: (Engagement, Double), ws: (Engagement, Double)*)  = binary(+|(w, ws:_*))
  def +|(w: (Engagement, Double), ws: (Engagement, Double)*)  = FreeArrow(WeightedCount((w +: ws).toMap))

  val oneFiveTwentyFive: EngagementWt = Map(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0)

  object ir   extends EvalRank[ResultsWithRelevant] {
    val ndcg2  = (p2 <<^ ((_: ResultsWithRelevant).labels)) >>> rank.ndcg
  }
  object rank extends EvalRank[LabelledIndexes] {
    val ndcg2  = p2 >>> ndcg
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
  )(implicit O: Order[F[_, _]], S: Show[F[_, _]], R: R[G], M: MonoidK[G[A, *]]): G[A, (String, B)] =
    combineMetrics[R, F, G, A, B](metrics, compiler)
      .rmap { case (k, v) => buildMetricKey(k, prefix, delim, suffix) -> v }

  def combineMetrics[R[f[_, _]] >: ACP[f] <: AR[f], F[_, _], G[_, _], A, B](
    metrics     : TraversableOnce[FreeArrow[R, F, A, B]],
    compiler    : F ~~> G
  )(implicit R: R[G], M: MonoidK[G[A, *]]): G[A, (List[F[_, _]], B)] =
    metrics.toList.foldMapK(_
      .summarize[List[F[_, _]]](analyze[F].list)
      .foldMap(compiler)
    )

  def buildMetricKey[F[_, _]](
    l: List[F[_, _]],
    prefix      : String = "",
    delim       : String = ".",
    suffix      : String = ""
  )(implicit O: Order[F[_, _]], S: Show[F[_, _]]): String =
    l.sorted(O.toOrdering).map(S.show).foldSmash(prefix, delim, suffix)

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

  def metricKeyBuilder[F[_, _]]
  (prefix: String = "", delim: String = ".", suffix: String = "")
  (implicit S: Show[F[_, _]], O: Order[F[_, _]])
  : OpLedger[F, *, *] ~~> RunMap =
    new (OpLedger[F, *, *] ~~> RunMap) {
      def apply[A, B](fab: OpLedger[F, A, B]): A => Map[String, B] =
        fab.run.rmap(_.map { case (k, v) => buildMetricKey(k, prefix, delim, suffix) -> v }.toMap)
    }


  type OpLedger[F[_, _], -A, B] = Kleisli[List, A, (List[F[_, _]], B)]

  implicit def arrowToKList[F[_, _]]: ArrowPlus[OpLedger[F, *, *]] = new ArrowPlus[OpLedger[F, -*, *]] {

    def zeroArrow[B, C]: OpLedger[F, B, C] = Kleisli(_ => Nil)

    def plus[A, B](f: OpLedger[F, A, B], g: OpLedger[F, A, B]): OpLedger[F, A, B] =
      Kleisli(a => f(a) ++ g(a))

    def lift[A, B](f: A => B): OpLedger[F, A, B] = Kleisli(a => List((Nil, f(a))))

    def compose[A, B, C](f: OpLedger[F, B, C], g: OpLedger[F, A, B]): OpLedger[F, A, C] =
      Kleisli { a => g(a).flatMap { case (k1, b) => f(b).map { case (k2, c) => (k1 ++ k2, c) } } }

    def first[A, B, C](fa: OpLedger[F, A, B]): OpLedger[F, (A, C), (B, C)] = fa.first[C].rmap { case ((k, b), c) => (k, b -> c) }
  }

  lazy implicit val eAR: AP[EvalOpLedger] = arrowToKList[EvalOp]

  val toEvalOpLedger: EvalOp ~~> EvalOpLedger = new (EvalOp ~~> EvalOpLedger) {
    def apply[A, B](fab: EvalOp[A, B]): EvalOpLedger[A, B] =
      Kleisli(fab(_).foldMapK(b => List((List(fab), b))))
  }

  def compileToEvaluator[R[f[_, _]] >: ACP[f] <: AR[f], A, B](
    fab: FreeArrow[R, EvalOp, A, B],
    prefix: String = "",
    delim: String = ".",
    suffix: String = ""
  )(implicit R: R[EvalOpLedger]): A => Map[String, B] = {
    val metricFormatter = metricKeyBuilder[EvalOp](prefix, delim, suffix)
    val evalFunction = fab.foldMap[EvalOpLedger](toEvalOpLedger)
    metricFormatter(evalFunction)
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
