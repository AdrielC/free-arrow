package com.adrielc.quivr

import simulacrum.typeclass

@typeclass trait ArrowApply[~>[_, _]] extends Arrow[~>] {

  def app[B, C]: (B ~> C, B) ~> C
}
