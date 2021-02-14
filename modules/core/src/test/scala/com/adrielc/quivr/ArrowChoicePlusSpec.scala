package com.adrielc.quivr

import org.scalatest.{FlatSpec, Matchers}

import data.Kleisli
import cats.implicits._
import implicits._

class ArrowChoicePlusSpec extends FlatSpec with Matchers {

  "ArrowChoicePlusSpec" should "add" in {

    val eval1 = Kleisli((i: Int) => List(i.toString + " intToString"))

    val eval2 = Kleisli((i: Int) => List(i.toLong + 100L))

    val res = eval1 |&| eval2

    assert(res.run(1) == List(Left("1 intToString"), Right(101L)))
  }
}
