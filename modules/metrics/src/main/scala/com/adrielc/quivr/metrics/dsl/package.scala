package com.adrielc.quivr.metrics

import cats.data.{Kleisli, NonEmptyList}
import cats.{Monoid, Order, Show}
import com.adrielc.quivr.free.{ACP, AP, AR, FA, FreeArrow}
import com.adrielc.quivr.{ArrowChoicePlus, ~>|, ~~>}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{EngagedResults, LabelledIndexes, ResultsWithRelevant}
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel
import com.adrielc.quivr.metrics.dsl.EvalOp.EngagementOp.EngagementToLabel._
import com.adrielc.quivr.metrics.dsl.EvalOp.Metric.{Gain, RankingMetric, RetrievalMetric}
import FreeArrow._
import com.adrielc.quivr.metrics.dsl.EvalOp.Metric

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

  object rank       extends MetricBuilder[LabelledIndexes]
  object retrieval  extends MetricBuilder[ResultsWithRelevant]

  val oneFiveTwentyFive : EngagementWt = Map(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0)
  val onlyPurchTwenty   : EngagementWt = Map(Purchase -> 20)
  val onlyPurchOne      : EngagementWt = Map(Purchase -> 1)
  val cartOnePurchTen   : EngagementWt = Map(CartAdd -> 1, Purchase -> 10)

  def +|(w: (Engagement, Double), ws: (Engagement, Double)*)  = liftK(WeightedCount((w +: ws).toMap))

  def !|(w: (Engagement, Double), ws: (Engagement, Double)*)  = liftK(Binary(+|(w, ws:_*)))

  def atK[A: ToK](k: Int) = FreeArrow(EvalOp.AtK(k))

  implicit class EngagementOps(private val e: Engagement) extends AnyVal {
    def +(other: Engagement): Labeler = +e + +other
    def /(other: Engagement): Labeler = e/other
    def unary_+ : Labeler             = liftK(Count(e))
    def unary_! : Labeler             = liftK(Binary(+e))
  }

  implicit class LabelerOps(private val l: Labeler) extends AnyVal {
    def +(other: Labeler): Labeler = liftK(Plus(l, other))
    def +(other: Engagement): Labeler = l + +other
    def unary_! : Labeler = !l

    private[metrics] def getLabeler: EngagementCounts => Option[Double] = l.analyze[GetLabel](new (EngagementToLabel ~>| GetLabel) {
      def apply[A, B](fab: EngagementToLabel[A, B]): GetLabel = fab.label
    })
  }

  implicit class EvalOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val e: FreeArrow[R, EvalOp, A, B]) extends AnyVal {

    def at(k: Int)(implicit T: ToK[B]): FreeArrow[R, EvalOp, A, B] =
      e >>> liftK(EvalOp.AtK[B](k))

    def at(k: Int, ks: Int*)(implicit T: ToK[B], P: <+>@[R]): FreeArrow[P.Lub, EvalOp, A, B] =
      e >>> plusAll(NonEmptyList(liftK(EvalOp.AtK[B](k)), ks.map(k => liftK(EvalOp.AtK[B](k))).toList))
  }

  implicit class MetricOps[R[f[_, _]] >: ACP[f] <: AR[f], A, B](private val e: FreeArrow[R, EvalOp, A, B]) extends AnyVal {

    def evalRanking(m: RankingMetric)(implicit T: IndexedLabels[B]): FreeArrow[R, EvalOp, A, Double] =
      e.rmap(_.labels) >>^ m

    def evalRanking(m: RankingMetric, ms: RankingMetric*)
                   (implicit T: IndexedLabels[B], P: <+>@[R]): FreeArrow[P.Lub, EvalOp, A, Double] =
      e.rmap(_.labels) >>> plusAll(NonEmptyList(m, ms.toList).map(liftK))

    def evalResults(m: RetrievalMetric)(implicit T: RelevantCount[B]): FreeArrow[R, EvalOp, A, Double] =
      e.rmap(_.relevanceCounts) >>^ m

    def evalResults(m: RetrievalMetric, ms: RetrievalMetric*)
                   (implicit T: RelevantCount[B], P: <+>@[R]): FreeArrow[P.Lub, EvalOp, A, Double] =
      e.rmap(_.relevanceCounts) >>> plusAll(NonEmptyList(m, ms.toList).map(liftK))
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

  implicit def arrowToKList[F[_, _]]: ArrowChoicePlus[OpLedger[F, *, *]] = new ArrowChoicePlus[OpLedger[F, -*, *]] {

    def choose[A, B, C, D](f: OpLedger[F, A, C])(g: OpLedger[F, B, D]): OpLedger[F, Either[A, B], Either[C, D]] =
      Kleisli {
        case Left(a)  => f(a).map { case (k, v) => k -> Left(v) }
        case Right(b) => g(b).map { case (k, v) => k -> Right(v) }
      }

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

  abstract class MetricBuilder[A: IndexedLabels : ToK] extends Serializable {
    import Metric._
    val ndcg              = lift((_: A).labels) >>^ Ndcg(Gain.Pow2)
    def ndcgWithGain(g: Gain)    = lift((_: A).labels) >>^ Metric.Ndcg(g)
    val averagePrecision  = lift((_: A).labels) >>^ AveragePrecision
    val reciprocalRank    = lift((_: A).labels) >>^ ReciprocalRank
    val rPrecision        = lift((_: A).labels) >>^ RPrecision
    val recall            = lift((_: A).relevanceCounts) >>^ Recall
    val precision         = lift((_: A).relevanceCounts) >>^ Precision
    val fScore            = lift((_: A).relevanceCounts) >>^ FScore
  }

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
}
