package com.adrielc.quivr.metrics.dsl

import cats.Order
import cats.instances.int._

sealed abstract class Engagement(val shortName: String) extends Product with Serializable
object Engagement {

  case object Click     extends Engagement("clk")
  case object CartAdd   extends Engagement("crt")
  case object Purchase  extends Engagement("prch")
  case object QuickView extends Engagement("qck")
  case object Favorite  extends Engagement("fav")
  case object Review    extends Engagement("rev")


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