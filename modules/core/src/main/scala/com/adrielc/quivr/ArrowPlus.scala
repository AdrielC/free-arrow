package com.adrielc.quivr

import cats.arrow.Arrow
import simulacrum.{op, typeclass}

@typeclass trait ArrowPlus[~>[_, _]] extends Arrow[~>] {

  @op("<+>", alias = true)
  def plus[A, B](f: A ~> B, g: A ~> B): A ~> B
}