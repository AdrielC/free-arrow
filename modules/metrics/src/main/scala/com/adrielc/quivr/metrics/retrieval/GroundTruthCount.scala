package com.adrielc.quivr.metrics.retrieval

import simulacrum.typeclass

@typeclass trait GroundTruthCount[A] {

  def groundTruthCount(a: A): Int
}
