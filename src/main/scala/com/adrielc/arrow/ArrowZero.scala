package com.adrielc.arrow

import cats.arrow.Arrow
import simulacrum.typeclass

@typeclass trait ArrowZero[~>[_, _]] extends Arrow[~>] {

  def zeroArrow[B, C]: B ~> C

  def ~@[B, C]: B ~> C = zeroArrow[B, C]
}
