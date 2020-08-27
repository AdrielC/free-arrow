package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import com.adrielc.arrow.metrics.EvalOp.Metric.Ndcg
import com.adrielc.arrow.metrics.evaluable.LabelledIndexes

class FreeMetricsTest extends FlatSpec with Matchers {

  "Metrics" should "accurately compute" in {

    val labelledIndexes = LabelledIndexes(
      1 -> 1.0,
      2 -> 2.0,
      3 -> 3.0,
      4 -> 4.0
    )

    val ndcg = Ndcg(labelledIndexes)

    println(ndcg)
  }
}
