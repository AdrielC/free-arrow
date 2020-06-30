package com.adrielc.arrows

import cats.arrow.Arrow
import simulacrum.typeclass

@typeclass trait ArrowZero[~>[_, _]] extends Arrow[~>] {

  def zeroArrow[B, C]: B ~> C
}