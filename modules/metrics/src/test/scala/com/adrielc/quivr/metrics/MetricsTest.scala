package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem, NonEmptySet => Nes}
import cats.implicits._
import com.adrielc.quivr.metrics.result.{Qrels, ResultLabels, Results}
import com.adrielc.quivr.metrics.types.QrelSet
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import org.scalatest.{FlatSpec, Matchers}

case class ResultsWithRelevant(results: Nel[Long], relevant: Nes[Long], labels: Nem[Long, Double])
object ResultsWithRelevant {
  implicit val resultSetResultsWithRelevant: Results[ResultsWithRelevant] = _.results
  implicit val groundTruthResultsWithRelevant: Qrels[ResultsWithRelevant] = a => QrelSet(a.relevant)
  implicit val resultLabelsResultsWithRelevant: ResultLabels[ResultsWithRelevant] = _.labels
}


class MetricsTest extends FlatSpec with Matchers {

  val results = ResultsWithRelevant(
    Nel.of(1L, 2L, 3L, 4L),
    Nes.of(1L, 2L, 3L, 4L),
    Nem.of(1L -> 1d, 2L -> 2d, 3L -> 3d, 4L -> 4d)
  )

  val relevanceJudgements: Nel[Boolean] = Nel.of(
    true,
    false,
    true,
    true,
    false,
    true,
    false,
    false
  )

  "Ndcg" should "accurately compute" in {

    assert(results.ndcg(gain.pow2).contains(0.6020905207089401))
    assert(results.ndcg(gain.id).contains(0.7489030296784172))
  }

  "Recall" should "accurately compute" in {

    assertResult(Some(0.25))(results.copy(relevant =  Nes.of(1L, 10L, 20L, 30L)).recall)
    assertResult(Some(0.50))(results.copy(relevant =  Nes.of(1L, 2L, 20L, 30L)).recall)
    assertResult(Some(0.75))(results.copy(relevant =  Nes.of(1L, 2L, 3L, 30L)).recall)
    assertResult(Some(1.00))(results.copy(relevant =  Nes.of(1L, 2L, 3L, 4L)).recall)
  }

  "Precision" should "accurately compute" in {

    assertResult(Some(0.25))(results.copy(relevant =  Nes.of(1L, 100L, 200L, 300L, 400L, 500L, 600L)).precision)
    assertResult(Some(0.50))(results.copy(relevant =  Nes.of(1L, 2L, 100L, 200L, 300L, 400L, 500L, 600L)).precision)
    assertResult(Some(0.75))(results.copy(relevant =  Nes.of(1L, 2L, 3L, 100L, 200L, 300L, 400L, 500L, 600L)).precision)
    assertResult(Some(1.00))(results.copy(relevant =  Nes.of(1L, 2L, 3L, 4L, 100L, 200L, 300L, 400L, 500L, 600L)).precision)
  }

  "FScore" should "accurately compute" in {

    // http://queirozf.com/entries/evaluation-metrics-for-ranking-problems-introduction-and-examples

//    1	06	0.90	Relevant (1.0)	0.40
//    2	03	0.85	Not Relevant (0.0)	0.33
//    3	05	0.71	Relevant (1.0)	0.57 (example link had math error)
//    4	00	0.63	Relevant (1.0)	0.75
//    5	04	0.47	Not Relevant (0.0)	0.66
//    6	02	0.36	Relevant (1.0)	0.80
//    7	01	0.24	Not Relevant (0.0)	0.73
//    8	07	0.16	Not Relevant (0.0)	0.66

    val expected = Nel.of(
      0.40,
      0.3333333333333333,
      0.5714285714285715,
      0.75,
      0.6666666666666665,
      0.80,
      0.7272727272727273,
      0.6666666666666666
    )

    expected.mapWithIndex((f, idx) => assert(relevanceJudgements.fScoreAtK(PosInt.unsafeFrom(idx + 1)).contains(f)))
  }


  "AP" should "accurately compute" in {

    // expected result taken from following link
    //https://queirozf.com/entries/evaluation-metrics-for-ranking-problems-introduction-and-examples
    //coincidentally their AP example had an arithmetic error, 0.77 is right, 0.80 is wrong

    assert(relevanceJudgements.averagePrecision.contains(0.7708333333333333))
  }

  "MRR" should "accurately compute" in {

    assert(relevanceJudgements.reciprocalRank.contains(1.0))
    assert(Nel.of(false, false, true, true, false, true, false, false).reciprocalRank.contains(0.3333333333333333))
  }

  "RPrecision" should "accurately compute" in {

    assert(relevanceJudgements.rPrecision == relevanceJudgements.precisionAtK(4))
    assert(relevanceJudgements.rPrecision.contains(0.75))
  }
}
