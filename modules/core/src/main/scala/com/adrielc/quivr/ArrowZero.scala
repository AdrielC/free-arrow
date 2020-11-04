package com.adrielc.quivr

import simulacrum.typeclass

@typeclass trait ArrowZero[~>[_, _]] extends ArrowPlus[~>] {

  def zeroArrow[B, C]: B ~> C

  def thenToZero[A, B, C](fab: A ~> B): A ~> C = andThen(fab, zeroArrow[B, C])
}