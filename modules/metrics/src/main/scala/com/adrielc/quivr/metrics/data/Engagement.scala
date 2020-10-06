package com.adrielc.quivr.metrics.data

import cats.Order
import cats.instances.int._

sealed trait Engagement extends Product with Serializable
object Engagement {

  case object Click     extends Engagement
  case object CartAdd   extends Engagement
  case object Purchase  extends Engagement

  implicit val orderEngagements: Order[Engagement] = Order.by {
    case Click    => 1
    case CartAdd  => 2
    case Purchase => 3
  }
}