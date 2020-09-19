package com.adrielc.quivr

import cats.Eval
import org.scalatest.{FlatSpec, Matchers}

import scala.Function.untupled
import com.adrielc.quivr.data._

class ArrowLoopSpec extends FlatSpec with Matchers {

  "ArrowLoop" should "compute fibonacci without overflow" in {

    val fib =
      untupled(ArrowLoop[Function1].loop[(Int, Int, Int), (Int, Int, Int), (Int, Int, Int) => (Int, Int, Int)](
        bd => lazyTuple(bd._2.value tupled bd._1.value, Eval.now({
          case r@(0, _, _) => r
          case (n, f0, f1) => bd._2.value(n - 1, f1, f1 + f0)
        }))
      ))(_: Int, 0, 1)._2

    assert(fib(40) == 102334155)
  }
}
