package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}

@typeclass trait ToK[A] {

  def toK(a: A, k: Int): Option[A]

  @op("maxK")
  def maxK(a: A): Int

  @op("filterToK")
  def filterToK(a: A, k: Int): Option[A] = if(k <= maxK(a)) toK(a, k) else None
}
