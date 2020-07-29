package com.adrielc.arrow.zio

import org.scalatest.{FlatSpec, Matchers}

class ZArrowSpec extends FlatSpec with Matchers {

  "ZArrow" should "not stack overflow" in {

    val add1 = ZArrow.lift((i: Int) => i + 1)

    var z = add1

    for (_ <- 1 until 10000) z = z.andThen(add1)

    val run = z.run(0)

    assert(zio.Runtime.default.unsafeRun(run) === 10000)
  }
}
