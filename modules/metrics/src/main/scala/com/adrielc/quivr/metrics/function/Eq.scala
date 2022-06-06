package com.adrielc.quivr.metrics.function

import scala.{Ordering => Ord}, Ord.Implicits._

sealed abstract class Eq[A](val f2: (A, A) => Boolean, str: String) extends Fn2[A, A, Boolean] {
  override def toString: String = str
}
private[metrics] object Eq {
  case class ===[A: Ord]() extends Eq[A](_ equiv _, "==")
  case class >[A: Ord]()   extends Eq[A](_ > _, ">")
  case class <[A: Ord]()   extends Eq[A](_ < _, "<")
  case class <=[A: Ord]()  extends Eq[A](_ <= _, "<=")
  case class >=[A: Ord]()  extends Eq[A](_ >= _, ">=")

  abstract class EqFor[A: Ord] {
    val `===`: Eq[A]  = Eq.===[A]
    val `>`: Eq[A]    = Eq.>[A]
    val `<`: Eq[A]    = Eq.<[A]
    val `<=`: Eq[A]   = Eq.<=[A]
    val `>=`: Eq[A]   = Eq.>=[A]
  }
}