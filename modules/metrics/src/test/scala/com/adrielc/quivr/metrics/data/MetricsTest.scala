package com.adrielc.quivr.metrics.data

import com.adrielc.quivr.metrics.dsl.EvalOp.RankingMetric.{Ndcg, Precision, Recall}
import org.scalatest.{FlatSpec, Matchers}

class MetricsTest extends FlatSpec with Matchers {

  val labelledIndexes = LabelledIndexes.of(
    1 -> 1.0,
    2 -> 2.0,
    3 -> 3.0,
    4 -> 4.0
  )

  val rankingRelevants = ResultsWithRelevant(
    1L, 2L, 3L, 4L
  )(
    1L, 10L, 20L, 30L
  )

  "Ndcg" should "accurately compute" in {

    assert(Ndcg(labelledIndexes).contains(0.7489030296784172))
  }

  "Recall" should "accurately compute" in {

    assertResult(Some(0.25))(
      Recall(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 10L, 20L, 30L))
    )

    assertResult(Some(0.50))(
      Recall(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 20L, 30L))
    )

    assertResult(Some(0.75))(
      Recall(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 30L))
    )
    assertResult(Some(1.00))(
      Recall(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 4L))
    )
  }

  "Precision" should "accurately compute" in {

    assertResult(Some(0.25))(
      Precision(ResultsWithRelevant(1L, 2L, 3L, 4L)(
        1L,
        100L, 200L, 300L, 400L, 500L, 600L
      ))
    )

    assertResult(Some(0.50))(
      Precision(ResultsWithRelevant(1L, 2L, 3L, 4L)(
        1L, 2L,
        100L, 200L, 300L, 400L, 500L, 600L
      ))
    )

    assertResult(Some(0.75))(
      Precision(ResultsWithRelevant(1L, 2L, 3L, 4L)(
        1L, 2L, 3L,
        100L, 200L, 300L, 400L, 500L, 600L
      ))
    )
    assertResult(Some(1.00))(
      Precision(ResultsWithRelevant(1L, 2L, 3L, 4L)(
        1L, 2L, 3L, 4L,
        100L, 200L, 300L, 400L, 500L, 600L
      ))
    )
  }
}
