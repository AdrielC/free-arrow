package com.adrielc.quivr.metrics

package object function {

  object gain {
    val pow2    : GainFn = GainFn.Pow2
    val pow1p1  : GainFn = GainFn.Pow1p1 // avoids overflow on larger label value ranges (>1000)
    val pow1p01 : GainFn = GainFn.Pow1p01
    val id      : GainFn = GainFn.Id
  }
  object discount {
    val log2    : DiscountFn = DiscountFn.Log2
    val id      : DiscountFn = DiscountFn.Id
  }

  object double extends Eq.EqFor[Double]
  object int extends Eq.EqFor[Int]
}

package function {

  private[function] trait Fn[-A, +B] extends Product with Serializable {
    def f: A => B; def apply(l: A): B = f(l)
  }
  private[function] trait Fn2[-A, -B, +C] extends Fn[(A, B), C] {
    def f: ((A, B)) => C = f2.tupled(_)
    def f2: (A, B) => C; def apply(a: A, b: B): C = f2(a, b)
  }
}
