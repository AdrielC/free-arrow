package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.Label

import Ordering.Implicits._
import scala.math.{Ordering => Ord}

object function {
  sealed trait Fn[-A, +B] extends Product with Serializable { def f: A => B; def apply(l: A): B = f(l) }
  sealed trait Fn2[-A, -B, +C] extends Fn[(A, B), C] { def f: ((A, B)) => C = f2.tupled(_);  def f2: (A, B) => C; def apply(a: A, b: B): C = f2(a, b) }

  sealed abstract class Gain(val f: Label => Label) extends Fn[Label, Label]
  object Gain {
    case object Pow2    extends Gain(pow2)
    case object Pow1p1  extends Gain(powOf(1.1)) // avoids overflow on larger label value ranges (>1000)
    case object Pow1p01 extends Gain(powOf(1.01)) // avoids overflow on larger label value ranges (>1000)
    case object Id      extends Gain(identity)
  }

  sealed abstract class Discount(val f: Int => Double) extends Fn[Int, Double]
  object Discount {
    case object Log2plus1 extends Discount(log2p1)
    case object Id        extends Discount(i => i.toDouble)
  }


  sealed abstract class Eq[A](val f2: (A, A) => Boolean) extends Fn2[A, A, Boolean]
  object Eq {
    case class ===[A: Ord]()  extends Eq[A](_ equiv _)
    case class >[A: Ord]()   extends Eq[A](_ > _)
    case class <[A: Ord]()   extends Eq[A](_ < _)
    case class <=[A: Ord]()  extends Eq[A](_ <= _)
    case class >=[A: Ord]()  extends Eq[A](_ >= _)


    abstract class EqFor[A: Ord] {
      val `===`: Eq[A]  = Eq.===[A]
      val `>`: Eq[A]   = Eq.>[A]
      val `<`: Eq[A]   = Eq.<[A]
      val `<=`: Eq[A]  = Eq.<=[A]
      val `>=`: Eq[A]  = Eq.>=[A]
    }


    object double extends EqFor[Double]
  }
}
