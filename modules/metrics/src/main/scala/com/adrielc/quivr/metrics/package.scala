package com.adrielc.quivr

import cats.Order
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Rank
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.{PosInt, PosLong}
import eu.timepit.refined.numeric._

import scala.math.{log, pow}
import scala.util.Try

package object metrics extends AtK.ToAtKOps
  with ResultSet.ToResultSetOps
  with TruePositiveCount.ToTruePositiveCountOps
  with RelevanceJudgements.ToRelevanceJudgementsOps
  with GroundTruthSet.ToGroundTruthSetOps
  with ResultLabels.ToResultLabelsOps
  with RelevantCounts.ToRelevantCountsOps
  with LabelledSet.ToLabelledSetOps  {

  implicit class EngCountOps(private val n: Int) {
    def clicks: EngagementCounts = EngagementCounts.clicks(PosLong.unsafeFrom(n.toLong))
    def click: EngagementCounts = EngagementCounts.clicks(PosLong.unsafeFrom(n.toLong))
    def cartAdds: EngagementCounts = EngagementCounts.cartAdds(PosLong.unsafeFrom(n.toLong))
    def cartAdd: EngagementCounts = EngagementCounts.cartAdds(PosLong.unsafeFrom(n.toLong))
    def purchases: EngagementCounts = EngagementCounts.purchases(PosLong.unsafeFrom(n.toLong))
    def purchase: EngagementCounts = EngagementCounts.purchases(PosLong.unsafeFrom(n.toLong))
  }

  private[metrics] val log2 = (d: Double) => log(d) / log(2.0)
  private[metrics] val log2p1 = (i: Rank) => 1 / log2(i + 1.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val calc = (a: Double, b: Int) => safeDiv(a, b.toDouble)
  private[metrics] val nonInf = (a: Double) => Some(a).filter(p => p != Double.NaN || p.abs != Double.PositiveInfinity)
  private[metrics] val safeDiv: (Double, Double) => Option[Double] = (a, b) => Try(a / b).toOption.flatMap(nonInf)
  private[metrics] def calcDcg(labels: NonEmptyList[Double], gain: function.Gain = function.Gain.Pow2): Option[Double] =
    nonInf(labels.foldLeft((0.0, 1)) { case ((s, idx), label) => (s + (gain(label) * log2p1(PosInt.unsafeFrom(idx))), idx + 1) }._1)
}

package metrics {

  import com.adrielc.quivr.metrics.data.{NonZeroCount, ResultId}

  sealed abstract class Engagement private (val order: Int) extends Product with Serializable
  object Engagement {
    case object QuickView extends Engagement(1)
    case object Click     extends Engagement(2)
    case object Favorite  extends Engagement(3)
    case object CartAdd   extends Engagement(4)
    case object Purchase  extends Engagement(5)
    case object Review    extends Engagement(6)

    implicit val engagementOrder: Order[Engagement] = Order.by(_.order)
    implicit val engagementOrdering: Ordering[Engagement] = engagementOrder.toOrdering
  }

  case class EngagedResults(results: NonEmptyList[ResultId], engagements: NonEmptyMap[ResultId, EngagementCounts])

  case class EngagementCounts(engagements: NonEmptyMap[Engagement, NonZeroCount]) {
    def +(other: EngagementCounts): EngagementCounts = EngagementCounts(NonEmptyMap.fromMapUnsafe(engagements.toSortedMap |+| other.engagements.toSortedMap))
  }
  object EngagementCounts {
    def apply(click: NonZeroCount, cart: NonZeroCount, purchase: NonZeroCount): EngagementCounts = clicks(click) + cartAdds(cart) + purchases(purchase)
    def clicks(n: NonZeroCount): EngagementCounts = EngagementCounts(NonEmptyMap.one(Engagement.Click, n))
    def cartAdds(n: NonZeroCount): EngagementCounts = EngagementCounts(NonEmptyMap.one(Engagement.CartAdd, n))
    def purchases(n: NonZeroCount): EngagementCounts = EngagementCounts(NonEmptyMap.one(Engagement.Purchase, n))
  }
}
