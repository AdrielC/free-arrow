package com.adrielc.quivr.metrics.data

import cats.Order
import cats.instances.int._
import com.adrielc.quivr.metrics.EngagementCounts

sealed trait Engagement extends Product with Serializable
object Engagement {

  def apply(click: Int = 0, cartAdd: Int = 0, purchase: Int = 0): EngagementCounts =
    clicks(click) ++ cartAdds(cartAdd) ++ purchases(purchase)

  def clicks[N : Numeric](n: N): EngagementCounts = guard(n)(Click)
  def cartAdds[N : Numeric](n: N): EngagementCounts = guard(n)(CartAdd)
  def purchases[N : Numeric](n: N): EngagementCounts = guard(n)(Purchase)

  case object Click     extends Engagement
  case object CartAdd   extends Engagement
  case object Purchase  extends Engagement

  implicit val orderEngagements: Order[Engagement] = Order.by {
    case Click    => 1
    case CartAdd  => 2
    case Purchase => 3
  }

  private def guard[N](n: N)(e: Engagement)(implicit N: Numeric[N]): EngagementCounts =
    if(N.gt(n, N.zero)) Map(e -> N.toLong(n)) else Map.empty
}