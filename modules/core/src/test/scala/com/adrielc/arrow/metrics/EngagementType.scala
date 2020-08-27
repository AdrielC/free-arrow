package com.adrielc.arrow.metrics

import com.adrielc.arrow.free.FreeA
import FreeA.liftK
import cats.Order
import cats.instances.int._
import com.adrielc.arrow.metrics.LabelOp.{Binary, CountOf, PercentOf}

sealed trait EngagementType extends Product with Serializable {
  def unary_! : FreeLabel[EngagementCounts, Long]                   = liftK(CountOf(this)) >>> liftK(Binary[Long])
  def unary_+ : FreeLabel[EngagementCounts, Long]                   = liftK(CountOf(this))
  def /(other: EngagementType): FreeLabel[EngagementCounts, Double] = liftK(PercentOf(this, other))
}
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