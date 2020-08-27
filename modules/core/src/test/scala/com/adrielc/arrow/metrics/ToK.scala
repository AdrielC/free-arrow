package com.adrielc.arrow.metrics

import simulacrum.{op, typeclass}

@typeclass trait ToK[A] {

  @op("toK")
  def toK(a: A, k: Int): Option[A]

  @op("maxK")
  def maxK(a: A): Int
}