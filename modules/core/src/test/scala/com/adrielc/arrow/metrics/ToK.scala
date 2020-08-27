package com.adrielc.arrow.metrics

import com.adrielc.arrow.metrics.EvalOp.K
import com.adrielc.arrow.metrics.ToK.{InsufficientSize, KFiltered}
import simulacrum.{op, typeclass}

@typeclass trait ToK[A] {

  def toK(a: A, k: Int): Option[A]

  @op("maxK")
  def maxK(a: A): Int

  @op("filterToK")
  def filterToK(a: A, k: Int): KFiltered[A] =
    (if(k <= maxK(a)) toK(a, k).map(K(k) -> _) else None)
      .toRight(InsufficientSize(k, maxK(a)))
}

object ToK {

  type KFiltered[A] = Either[InsufficientSize, (K, A)]
  case class InsufficientSize(minRequired: Int, maxPossible: Int)
}
