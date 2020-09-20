package com.adrielc.quivr.metrics

import cats.Order
import cats.instances.int._

sealed trait EngagementType extends Product with Serializable
object EngagementType {
  case object Click     extends EngagementType
  case object CartAdd   extends EngagementType
  case object Purchase  extends EngagementType

  implicit val orderEngagements: Order[EngagementType] = Order.by {
    case Click    => 1
    case CartAdd  => 2
    case Purchase => 3
  }
}