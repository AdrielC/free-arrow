package com.adrielc.quivr

import cats.data.Kleisli
import cats.{Monad, MonoidK}
import simulacrum.{op, typeclass}

@typeclass trait ArrowPlus[~>[_, _]] extends ArrowZero[~>] {

  @op("<+>", alias = true)
  def plus[A, B](f: A ~> B, g: A ~> B): A ~> B
}

object ArrowPlus {

  implicit def kleisliArrowPlus[M[_]: Monad : MonoidK]: ArrowPlus[Kleisli[M, *, *]] = ArrowChoicePlus.arrowChoicePlusForKleisli
}