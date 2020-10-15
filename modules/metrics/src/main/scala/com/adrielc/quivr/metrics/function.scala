package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.data.{Label, Rank}

object function {
  sealed trait Fn[-A, +B] extends Product with Serializable { def apply(l: A): B }

  sealed abstract class Gain(f: Label => Label) extends Fn[Label, Label] { def apply(l: Label): Label = f(l) }
  object Gain {
    case object Pow2    extends Gain(pow2)
    case object Pow1p1  extends Gain(powOf(1.1)) // avoids overflow on larger label value ranges (>1000)
    case object Pow1p01 extends Gain(powOf(1.01)) // avoids overflow on larger label value ranges (>1000)
    case object None    extends Gain(identity)
  }

  sealed abstract class Discount(f: Rank => Double) extends Fn[Rank, Double] { def apply(l: Rank): Double = f(l) }
  object Discount {
    case object Log2plus1 extends Discount(log2p1)
  }

}
