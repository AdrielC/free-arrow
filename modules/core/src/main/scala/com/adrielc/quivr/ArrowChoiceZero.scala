package com.adrielc.quivr

import simulacrum.typeclass

@typeclass trait ArrowChoiceZero[~>[_, _]] extends ArrowZero[~>] with ArrowChoicePlus[~>] {

  def justLeft[A, B]: Either[B, A] ~> B = choice(id, zeroArrow)

  def justRight[A, B]: Either[A, B] ~> B = choice(zeroArrow, id)
}