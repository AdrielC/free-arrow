package com.adrielc.quivr.free

import cats.data.AndThen
import com.adrielc.quivr.BiFunctionK
import org.scalatest.{FlatSpec, Matchers}

class FreeComposeSpec extends FlatSpec with Matchers {

  "FreeCompose" should "not stack overflow" in {

    val add1 = FreeCompose.liftK((i: Int) => i + 1)

    val run = List.fill(100000)(add1).reduce(_ andThen _).foldMap(BiFunctionK.lift(AndThen.apply)).apply(0)

    assert(run == 100000)
  }
}
