package com.adrielc.quivr.metrics

import cats.data.NonEmptyList
import com.adrielc.quivr.data.Pure
import com.adrielc.quivr.metrics.EngagementType.Purchase
import com.adrielc.quivr.metrics.EvalOp.free._
import com.adrielc.quivr.metrics.LabelOp.free._
import com.adrielc.quivr.metrics.evaluable.{LabelledIndexes, ResultsWithEngagements}
import org.scalatest.{FlatSpec, Matchers}

import cats.implicits._

class FreeEvalTest extends FlatSpec with Matchers {

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

    val eval = engToLabel.inl >>> metric.right.inr

    assert(eval.foldMap(Pure[LabelOp] or Pure[EvalOp]).apply(results) == Right(0.9488107485678984))
  }
}

