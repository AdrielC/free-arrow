package com.adrielc.quivr.metrics.data

import cats.Order
import cats.implicits._
import eu.timepit.refined.types.numeric.PosLong

sealed abstract class Engagement extends Product with Serializable
object Engagement {
  case object Impression extends Engagement
  case object QuickView extends Engagement
  case object Click     extends Engagement
  case object Favorite  extends Engagement
  case object CartAdd   extends Engagement
  case object Purchase  extends Engagement
  case object Review    extends Engagement

  implicit val engagementOrder: Order[Engagement] = Order.by {
    case Impression   => 1
    case QuickView    => 2
    case Click        => 3
    case Favorite     => 4
    case CartAdd      => 5
    case Purchase     => 6
    case Review       => 7
  }
  implicit val engagementOrdering: Ordering[Engagement] = engagementOrder.toOrdering

  trait EngCountSyntax {
    implicit def toEngCountOps(n: Int): EngCountOps = new EngCountOps(n)
  }

  class EngCountOps(private val n: Int) extends AnyVal {
    def clicks: EngagementCounts = EngagementCounts.clicks(PosLong.unsafeFrom(n.toLong))
    def click: EngagementCounts = EngagementCounts.clicks(PosLong.unsafeFrom(n.toLong))
    def cartAdds: EngagementCounts = EngagementCounts.cartAdds(PosLong.unsafeFrom(n.toLong))
    def cartAdd: EngagementCounts = EngagementCounts.cartAdds(PosLong.unsafeFrom(n.toLong))
    def purchases: EngagementCounts = EngagementCounts.purchases(PosLong.unsafeFrom(n.toLong))
    def purchase: EngagementCounts = EngagementCounts.purchases(PosLong.unsafeFrom(n.toLong))
  }
}