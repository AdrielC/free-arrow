package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.EvalOp.Metric.Ndcg
import com.adrielc.quivr.metrics.evaluable.LabelledIndexes
import org.scalatest.{FlatSpec, Matchers}

class FreeMetricsTest extends FlatSpec with Matchers {

  "Metrics" should "accurately compute" in {

    val labelledIndexes = LabelledIndexes.of(
      1 -> 1.0,
      2 -> 2.0,
      3 -> 3.0,
      4 -> 4.0
    )

    val ndcg = Ndcg(labelledIndexes)

    assert(ndcg == 0.6020905207089401)
  }
}
