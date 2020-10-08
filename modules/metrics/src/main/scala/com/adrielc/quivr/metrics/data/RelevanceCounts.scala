package com.adrielc.quivr.metrics.data

import com.adrielc.quivr.metrics.RelevantCount

case class RelevanceCounts(
  nResults: Int,
  nRelevant: Int,
  nRelevantResults: Int
)
object RelevanceCounts {
  implicit val relevanceCount: RelevantCount[RelevanceCounts] = new RelevantCount[RelevanceCounts] {
    override def nRelevantResults(a: RelevanceCounts): Int = a.nRelevantResults
    override def nRelevant(a: RelevanceCounts): Int = a.nRelevant
    override def nResults(a: RelevanceCounts): Int = a.nResults
  }
}
