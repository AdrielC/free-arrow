package com.adrielc.arrow

import simulacrum.{op, typeclass}

@typeclass trait ArrowPlus[~>[_, _]] extends ArrowZero[~>] {

  @op("<+>", alias = true)
  def plus[A, B](f: A ~> B, g: A ~> B): A ~> B
}

object ArrowPlus {

  implicit val partialFunction: ArrowPlus[PartialFunction] = ArrowChoicePlus.partialFunction
}
