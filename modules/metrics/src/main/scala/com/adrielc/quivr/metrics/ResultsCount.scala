package com.adrielc.quivr.metrics

import simulacrum.{op, typeclass}

@typeclass trait ResultsCount[A] extends Serializable {

  @op("nResults")
  def nResults(a: A): Int
}
