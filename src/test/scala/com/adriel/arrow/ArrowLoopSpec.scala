package com.adriel.arrow

import com.adrielc.arrow.{ArrowLoop, LazyTuple}
import org.scalatest.{FlatSpec, Matchers}
import Function.untupled

class ArrowLoopSpec extends FlatSpec with Matchers {

  "ArrowLoop" should "compute fibonacci" in {

    val fib =
      untupled(ArrowLoop[Function1].loop[(Int, Int, Int), (Int, Int, Int), (Int, Int, Int) => (Int, Int, Int)](
        bd => LazyTuple(bd._2 tupled bd._1, {
          case r@(0, _, _) => r
          case (n, f0, f1) => bd._2(n - 1, f1, f1 + f0)
        })
      ))(_: Int, 0, 1)._2

    assert(fib(40) == 102334155)
  }
}
