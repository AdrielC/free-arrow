package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}

@typeclass trait ToK[A] {

  @op("toK")
  def setK(a: A, k: Int): Option[A]
}
