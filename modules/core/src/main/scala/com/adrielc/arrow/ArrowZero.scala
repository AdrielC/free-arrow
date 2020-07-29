package com.adrielc.arrow

import cats.{Monad, MonoidK}
import cats.arrow.Arrow
import cats.data.Kleisli
import simulacrum.typeclass

@typeclass trait ArrowZero[~>[_, _]] extends Arrow[~>] {

  def zeroArrow[B, C]: B ~> C
}

object ArrowZero {

  implicit def kleisliArrowZero[M[_]: Monad : MonoidK]: ArrowZero[Kleisli[M, ?, ?]] = ArrowChoicePlus.arrowChoicePlusForKleisli
}