package com.adrielc.quivr.metrics

import cats.Order
import cats.data.NonEmptyMap
import cats.implicits._
import com.adrielc.quivr.metrics.data.{EngagedResults, KeyCounts}
import com.adrielc.quivr.metrics.dsl.engagement
import eu.timepit.refined.types.numeric.PosInt

sealed abstract class MyEngagement extends Product with Serializable
object MyEngagement {
  import dsl._
  type ResultEngs = EngagedResults[MyEngagement]

  val clicks    : engagement.Labeler[MyEngagement] = label.count(Click: MyEngagement)
  val cartAdds  : engagement.Labeler[MyEngagement] = label.count(CartAdd: MyEngagement)
  val purchases : engagement.Labeler[MyEngagement] = label.count(Purchase: MyEngagement)
  val anyClicks    : engagement.Judge[MyEngagement] = judge.any(Click: MyEngagement)
  val anyCartAdds  : engagement.Judge[MyEngagement] = judge.any(CartAdd: MyEngagement)
  val anyPurchases : engagement.Judge[MyEngagement] = judge.any(Purchase: MyEngagement)


  type EngagementCounts = KeyCounts[MyEngagement]
  object EngagementCounts {

    def apply(click: NonZeroCount, cart: NonZeroCount, purchase: NonZeroCount): EngagementCounts =
      clicks(click) + cartAdds(cart) + purchases(purchase)

    def clicks(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(MyEngagement.Click, n))

    def cartAdds(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(MyEngagement.CartAdd, n))

    def purchases(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(MyEngagement.Purchase, n))
  }

  case object Impression extends MyEngagement
  case object QuickView extends MyEngagement
  case object Click     extends MyEngagement
  case object Favorite  extends MyEngagement
  case object CartAdd   extends MyEngagement
  case object Purchase  extends MyEngagement
  case object Review    extends MyEngagement

  implicit val engagementOrder: Order[MyEngagement] = Order.by {
    case Impression   => 1
    case QuickView    => 2
    case Click        => 3
    case Favorite     => 4
    case CartAdd      => 5
    case Purchase     => 6
    case Review       => 7
  }
  implicit val engagementOrdering: Ordering[MyEngagement] = engagementOrder.toOrdering

  implicit class EngCountOps(private val n: Int) extends AnyVal {
    def clicks: EngagementCounts = EngagementCounts.clicks(PosInt.unsafeFrom(n.toInt))
    def click: EngagementCounts = EngagementCounts.clicks(PosInt.unsafeFrom(n.toInt))
    def cartAdds: EngagementCounts = EngagementCounts.cartAdds(PosInt.unsafeFrom(n.toInt))
    def cartAdd: EngagementCounts = EngagementCounts.cartAdds(PosInt.unsafeFrom(n.toInt))
    def purchases: EngagementCounts = EngagementCounts.purchases(PosInt.unsafeFrom(n.toInt))
    def purchase: EngagementCounts = EngagementCounts.purchases(PosInt.unsafeFrom(n.toInt))
  }
}