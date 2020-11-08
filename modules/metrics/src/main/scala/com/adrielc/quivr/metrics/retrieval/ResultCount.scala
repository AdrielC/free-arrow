package com.adrielc.quivr.metrics.retrieval

import simulacrum.typeclass

@typeclass trait ResultCount[A] extends Serializable {

  // Number of results retrieved in this result set
  def resultCount(a: A): Int
}