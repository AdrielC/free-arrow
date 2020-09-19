package com.adrielc.quivr

import cats.arrow.ArrowChoice
import simulacrum.typeclass

@typeclass trait ArrowChoiceZero[~>[_, _]] extends ArrowChoice[~>] with ArrowZero[~>] {

  def justLeft[A, B]: Either[B, A] ~> B = choice(id, zeroArrow)

  def justRight[A, B]: Either[A, B] ~> B = choice(zeroArrow, id)
}