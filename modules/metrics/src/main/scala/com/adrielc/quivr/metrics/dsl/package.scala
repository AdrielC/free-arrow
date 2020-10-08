package com.adrielc.quivr.metrics

import cats.data.{Kleisli}
import cats.{Monoid, Order, Show}
import com.adrielc.quivr.free.{ACP, AP, AR, FA, FreeArrow}
import com.adrielc.quivr.{ArrowPlus, ~>|, ~~>}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{EngagedResults, LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel._
import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.Gain

package object dsl
  extends RelevantCount.ToRelevantCountOps
    with IndexedLabels.ToIndexedLabelsOps
    with ToK.ToToKOps {

  type Labeler            = FA[EngagementToLabel, EngagedResults, LabelledIndexes]
  type GetLabel           = EngagementCounts => Option[Double]
  type EvalOpLedger[A, B] = OpLedger[EvalOp, A, B]
  type RunMap[A, B]       = A => Map[String, B]
  type EngagementWt = Map[Engagement, Double]
  type OpLedger[F[_, _], -A, B] = Kleisli[List, A, (List[F[_, _]], B)]
  type EvalMap[+A] = Map[List[EvalOp[_, _]], A]

  val Click     : Engagement = Engagement.Click
  val CartAdd   : Engagement = Engagement.CartAdd
  val Purchase  : Engagement = Engagement.Purchase
  val QuickView : Engagement = Engagement.QuickView
  val Favorite  : Engagement = Engagement.Favorite
  val Review    : Engagement = Engagement.Review

  val oneFiveTwentyFive : EngagementWt = Map(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0)
  val onlyPurchTwenty   : EngagementWt = Map(Purchase -> 20)
  val onlyPurchOne      : EngagementWt = Map(Purchase -> 1)
  val cartOnePurchTen   : EngagementWt = Map(CartAdd -> 1, Purchase -> 10)

  val binary    = (a: Labeler) => FreeArrow(Binary(a))
  val count     = (e: Engagement) => FreeArrow(Count(e))
  val plus      = (a: Labeler, b: Labeler) => FreeArrow(Plus(a, b))
  val percentOf = (a: Engagement, b: Engagement) => FreeArrow(PercentOf(a, b))

  def !|(w: (Engagement, Double), ws: (Engagement, Double)*)  = binary(+|(w, ws:_*))

  def +|(w: (Engagement, Double), ws: (Engagement, Double)*)  = FreeArrow(WeightedCount((w +: ws).toMap))

  object ir   extends EvalRank[ResultsWithRelevant]
  object rank extends EvalRank[LabelledIndexes]

  val Pow1p01 : Gain = Gain.Pow1p01
  val Pow1p1  : Gain = Gain.Pow1p1
  val Pow2    : Gain = Gain.Pow2

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

  implicit class RichNumeric[N](n: N)(implicit N: Numeric[N]) {
    def binarize: N = if(N.gt(n, N.zero)) N.one else N.zero
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

  private[metrics] implicit val moniodLabeler: Monoid[GetLabel] =
    Monoid.instance(_ => Some(0.0), (a, b) => e => a(e) |+| b(e))

  private[metrics] def metricKeyBuilder[F[_, _]](prefix: String = "", delim: String = ".", suffix: String = "")
                                                (implicit S: Show[F[_, _]], O: Order[F[_, _]]): OpLedger[F, *, *] ~~> RunMap =
    new (OpLedger[F, *, *] ~~> RunMap) {
      def apply[A, B](fab: OpLedger[F, A, B]): A => Map[String, B] =
        fab.run.rmap(_.map { case (k, v) =>
          k.sorted(O.toOrdering).map(S.show).foldSmash(prefix, delim, suffix) -> v
        }.toMap)
    }

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

  private[metrics] val toEvalOpLedger: EvalOp ~~> EvalOpLedger = new (EvalOp ~~> EvalOpLedger) {
    def apply[A, B](fab: EvalOp[A, B]): EvalOpLedger[A, B] =
      Kleisli(fab(_).foldMapK(b => List((List(fab), b))))
  }
}

package dsl {

  import cats.Order
  import cats.instances.int._
  import com.adrielc.quivr.metrics.dsl.EvalOp.MetricOp.{Discount, Gain}

  sealed abstract class Engagement(val shortName: String) extends Product with Serializable
  object Engagement {

    case object Click     extends Engagement("click")
    case object CartAdd   extends Engagement("cart")
    case object Purchase  extends Engagement("purchase")
    case object QuickView extends Engagement("quickView")
    case object Favorite  extends Engagement("favorite")
    case object Review    extends Engagement("review")

    def engagementsOrder(
      click     : Int = 1,
      cartAdd   : Int = 1,
      purchase  : Int = 1,
      quickView : Int = 1,
      favorite  : Int = 1,
      review    : Int = 1
    ): Order[Engagement] = Order.by {
      case Click      => click
      case CartAdd    => cartAdd
      case Purchase   => purchase
      case QuickView  => quickView
      case Favorite   => favorite
      case Review     => review
    }
  }

  abstract class EvalRank[A: IndexedLabels : ToK] extends Serializable {
    val ndcg = FreeArrow(EvalOp.MetricOp.Ndcg[A]())
    def ndcg2(g: Gain = Pow2, d: Discount = Discount.Log2p1) = FreeArrow(EvalOp.MetricOp.Ndcg[A](g, d))
    def atK(k: Int) = FreeArrow(EvalOp.AtK[A](k))
    val recall      = FreeArrow(EvalOp.MetricOp.Recall[A])
    val precision   = FreeArrow(EvalOp.MetricOp.Precision[A])
    val rPrecision  = FreeArrow(EvalOp.MetricOp.RPrecision[A])
    val fScore      = FreeArrow(EvalOp.MetricOp.FScore[A])
  }
}
