package com.adrielc.quivr.metrics

import cats.data.{NonEmptyMap => Nem, NonEmptySet => Nes, NonEmptyVector => Nev}
import cats.implicits._
import com.adrielc.quivr.metrics.result.Results.NonEmptyResults
import com.adrielc.quivr.metrics.result._
import function.gain
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import org.scalatest.{FlatSpec, Matchers}

case class ResultsWithRelevant(results: Nev[Long], relevant: Nes[Long], labels: Nem[Long, Label])
object ResultsWithRelevant {
  implicit val relevanciesInstance: NonEmptyResults[ResultsWithRelevant] with GroundTruth[ResultsWithRelevant] with ResultLabels[ResultsWithRelevant] =
    new NonEmptyResults[ResultsWithRelevant] with GroundTruth[ResultsWithRelevant] with ResultLabels[ResultsWithRelevant] {
      override def groundTruth(a: ResultsWithRelevant): GroundTruth.Set = GroundTruth.Set(a.relevant)
      override def nonEmptyResults(a: ResultsWithRelevant): Nev[ResultId] = a.results
      override def resultLabels(a: ResultsWithRelevant): Nem[ResultId, Label] = a.labels
  }
}


class MetricsTest extends FlatSpec with Matchers {
  import implicits._

  val results: ResultsWithRelevant = ResultsWithRelevant(
    Nev.of(1L, 2L, 3L, 4L),
    Nes.of(1L, 2L, 3L, 4L),
    Nem.of(1L -> 1d, 2L -> 2d, 3L -> 3d, 4L -> 4d)
  )

  val relevanceJudgements = Nev.of(true, false, true, true, false, true, false, false)

  "Ndcg" should "accurately compute" in {

    assert(results.ndcg(gain.pow2).contains(0.6020905207089401))
    assert(results.ndcg(gain.id).contains(0.7489030296784172))
    assert(results.ndcgK(4, gain.id).contains(0.7489030296784172))
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

    val expected = Nev.of(
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

    assert(relevanceJudgements.averagePrecisionK(8) == relevanceJudgements.averagePrecision)
  }

  "MRR" should "accurately compute" in {

    assert(relevanceJudgements.reciprocalRank.contains(1.0))

    val judged = Nev.of(false, false, true, true, false, true, false, false)

    assert(judged.reciprocalRank.contains(0.3333333333333333))
  }

  "RPrecision" should "accurately compute" in {

    assert(relevanceJudgements.rPrecision == relevanceJudgements.precisionAtK(4))
    assert(relevanceJudgements.rPrecision.contains(0.75))
  }
}
