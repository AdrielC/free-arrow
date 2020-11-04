package com.adrielc.quivr

import cats.arrow.Arrow
import simulacrum.{op, typeclass}

@typeclass trait ArrowParallel[~>[_, _]] extends Arrow[~>] {

  @op("|***|", alias = true)
  def splitPar[A, B, C, D](fab: A ~> B, fcd: C ~> D): (A, C) ~> (B, D)

  @op("|&&&|", alias = true)
  def mergePar[A, B, C](f: A ~> B, g: A ~> C): A ~> (B, C) =
    lmap(splitPar(f, g))((a: A) => (a, a))
}
