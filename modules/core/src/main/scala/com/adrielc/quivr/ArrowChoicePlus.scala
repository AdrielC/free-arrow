package com.adrielc.quivr

import simulacrum.{op, typeclass}
import cats.syntax.either._

@typeclass trait ArrowChoicePlus[~>[_, _]] extends ArrowChoice[~>] with  ArrowPlus[~>] {

  @op("|&|", alias = true)
  def and[A, B, C](f: A ~> B, g: A ~> C): A ~> Either[B, C] =
    andThen(plus(lift(_.asLeft[A]), lift(_.asRight[A])), choose(f)(g))
}