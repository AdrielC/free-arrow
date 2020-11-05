package com.adrielc.quivr.metrics

import Ordering.Implicits._
import scala.math.{Ordering => Ord}

object function {

  object gain {
    val pow2    : GainFn = GainFn.Pow2
    val pow1p1  : GainFn = GainFn.Pow1p1 // avoids overflow on larger label value ranges (>1000)
    val pow1p01 : GainFn = GainFn.Pow1p01
    val id      : GainFn = GainFn.Id
  }
  object discount {
    val log2    : Discount = Discount.Log2
    val id      : Discount = Discount.Id
  }

  object double extends Eq.EqFor[Double]
  object int extends Eq.EqFor[Int]
}

private[metrics] sealed trait Fn[-A, +B] extends Product with Serializable { def f: A => B; def apply(l: A): B = f(l) }

private[metrics] sealed trait Fn2[-A, -B, +C] extends Fn[(A, B), C] {
  def f: ((A, B)) => C = f2.tupled(_);  def f2: (A, B) => C; def apply(a: A, b: B): C = f2(a, b) }

sealed abstract class GainFn(val f: Double => Double) extends Fn[Double, Double]
private[metrics] object GainFn {
  case object Pow2    extends GainFn(pow2)
  case object Pow1p1  extends GainFn(powOf(1.1))
  case object Pow1p01 extends GainFn(powOf(1.01))
  case object Id      extends GainFn(identity)
}

sealed abstract class Discount(val f: Int => Double) extends Fn[Int, Double]
private[metrics] object Discount {

  //  we let a = 2 because it has been reported that nDCG with a large logarithm
  //  base is counterintuitive and lacks discriminative power (Sakai 2007d), despite
  //  the fact that this parameter was designed to reflect persis- tence

  case object Log2      extends Discount(log2)
  case object Id        extends Discount(i => i.toDouble)
}


sealed abstract class Eq[A](val f2: (A, A) => Boolean, str: String) extends Fn2[A, A, Boolean] {
  override def toString: String = str
}
private[metrics] object Eq {
  case class ===[A: Ord]()  extends Eq[A](_ equiv _, "=")
  case class >[A: Ord]()   extends Eq[A](_ > _, ">")
  case class <[A: Ord]()   extends Eq[A](_ < _, "<")
  case class <=[A: Ord]()  extends Eq[A](_ <= _, "<=")
  case class >=[A: Ord]()  extends Eq[A](_ >= _, ">=")


  abstract class EqFor[A: Ord] {
    val `===`: Eq[A]  = Eq.===[A]
    val `>`: Eq[A]   = Eq.>[A]
    val `<`: Eq[A]   = Eq.<[A]
    val `<=`: Eq[A]  = Eq.<=[A]
    val `>=`: Eq[A]  = Eq.>=[A]
  }
}




