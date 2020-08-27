package com.adrielc.arrow.metrics

import cats.data.NonEmptyList
import cats.implicits._
import com.adrielc.arrow.data.Pure
import com.adrielc.arrow.metrics.EngagementType.Purchase
import com.adrielc.arrow.metrics.EvalOp.free._
import com.adrielc.arrow.metrics.LabelOp.MissingLabels
import com.adrielc.arrow.metrics.LabelOp.free._
import com.adrielc.arrow.metrics.evaluable.{LabelledIndexes, ResultsWithEngagements}
import org.scalatest.{FlatSpec, Matchers}

class FreeEvalTest extends FlatSpec with Matchers {

  // {\displaystyle 3,3,3,2,2,2,1,0}

  val labelledIndexes = LabelledIndexes.of(
    1 -> 3,
    2 -> 2,
    3 -> 3,
    4 -> 0,
    5 -> 1,
    6 -> 2
  )

  val labelledIndexes2 = LabelledIndexes.of(
    5 -> 1.0,
    1 -> 7.0,
    6 -> 3.0,
    2 -> 3.0,
    3 -> 7.0
  )

  val results = ResultsWithEngagements(
    NonEmptyList.of(1L, 2L, 3L, 4L, 5L, 6L),
    Map(
      1L -> 3.purchases,
      2L -> 2.purchases,
      3L -> 3.purchases,
      5L -> 1.purchases,
      6L -> 2.purchases
    )
  )

  "Free Eval" should "evaluate correctly" in {

    val engToLabel = (countOf(Purchase) >>^ (_.toDouble) >>> pow2).forEngagedResults

    val metric = ndcg.tap(println)

    val eval = engToLabel.inl[EvalOp] >>> metric.right[MissingLabels].inr[LabelOp]

    assert(eval.foldMap(Pure[LabelOp] or Pure[EvalOp]).apply(results) == Right(0.9488107485678984))
  }
}

