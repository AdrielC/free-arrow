package com.adrielc.quivr.metrics.data

import com.adrielc.quivr.metrics.dsl.EvalOp.Metric.Gain
import org.scalatest.{FlatSpec, Matchers}

class MetricsTest extends FlatSpec with Matchers {

  "Ndcg" should "accurately compute" in {

    assert(LabelledIndexes.labels(1.0, 2.0, 3.0, 4.0).ndcg(Gain.Pow2.f).contains(0.6020905207089401))
    assert(LabelledIndexes.labels(1.0, 2.0, 3.0, 4.0).ndcg(Gain.Id.f).contains(0.7489030296784172))
  }

  "Recall" should "accurately compute" in {

    assertResult(Some(0.25))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 10L, 20L, 30L).recall)
    assertResult(Some(0.50))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 20L, 30L).recall)
    assertResult(Some(0.75))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 30L).recall)
    assertResult(Some(1.00))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 4L).recall)
  }

  "Precision" should "accurately compute" in {

    assertResult(Some(0.25))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 100L, 200L, 300L, 400L, 500L, 600L).precision)
    assertResult(Some(0.50))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 100L, 200L, 300L, 400L, 500L, 600L).precision)
    assertResult(Some(0.75))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 100L, 200L, 300L, 400L, 500L, 600L).precision)
    assertResult(Some(1.00))(ResultsWithRelevant(1L, 2L, 3L, 4L)(1L, 2L, 3L, 4L, 100L, 200L, 300L, 400L, 500L, 600L).precision)
  }

  "FScore" should "accurately compute" in {

    // http://queirozf.com/entries/evaluation-metrics-for-ranking-problems-introduction-and-examples

//    1	06	0.90	Relevant (1.0)	0.40
//    2	03	0.85	Not Relevant (0.0)	0.33
//    3	05	0.71	Relevant (1.0)	0.62
//    4	00	0.63	Relevant (1.0)	0.75
//    5	04	0.47	Not Relevant (0.0)	0.66
//    6	02	0.36	Relevant (1.0)	0.80
//    7	01	0.24	Not Relevant (0.0)	0.73
//    8	07	0.16	Not Relevant (0.0)	0.66

    val results = LabelledIndexes.labels(1, 0, 1, 1, 0, 1, 0, 0)

    assert(results.toK(4).flatMap(_.fScore).contains(0.75))
  }


  "AP" should "accurately compute" in {

    // expected result taken from following link
    //https://queirozf.com/entries/evaluation-metrics-for-ranking-problems-introduction-and-examples
    //coincidentally their AP example had an arithmetic error, 0.77 is right, 0.80 is wrong

    val results = LabelledIndexes.labels(1, 0, 1, 1, 0, 1, 0, 0)

    assert(results.averagePrecision.contains(0.7708333333333333))
  }

  "MRR" should "accurately compute" in {

    val results = LabelledIndexes.labels(0, 0, 1, 1, 0, 1, 0, 0)

    assert(results.reciprocalRank.contains(0.3333333333333333))
  }

  "RPrecision" should "accurately compute" in {

    val results = LabelledIndexes.labels(0, 1, 1, 0, 0, 1, 0, 0)

    assert(results.rPrecision.contains(0.6666666666666666))
  }
}
