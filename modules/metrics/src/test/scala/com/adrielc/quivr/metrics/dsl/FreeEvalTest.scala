package com.adrielc.quivr
package metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data._
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import MyEngagement._
import com.adrielc.quivr.metrics.dsl.evaluation.{KGreaterThanMax, NoValidJudgements}


class FreeEvalTest extends FlatSpec with Matchers {

  "Eval" should "run getting started" in {

    sealed trait Eng // user defined engagement domain
    case object Clicks extends Eng
    val clicks = label.of(Clicks: Eng)
    val anyClicks = judge.any(Clicks: Eng)

    type Res = (NonEmptyList[ResultId], Map[ResultId, Map[Eng, Int]]) // user defined results type

    val results: Res = (
      NonEmptyList.of(1L, 2L to 59L:_*), // results
      Map(2L -> Map((Clicks: Eng) -> 1))        // engagement counts
    )

    val evaluation =
      (judge.from[Res](anyClicks, clicks > 10) <+> clicks.from[Res]) >>>  // create continuous labels from click counts and binary judgements if has any clicks
        atK(10, 60) >++                                                   // compute downstream metrics for each K
        (eval.ndcg, eval.reciprocalRank)                                  // compute each metric

    val metrics = evaluation.run(results)

    assert(metrics ==
      NonEmptyMap.of(
        "judge((clicks>0)).mrr.@10" -> Right(0.5),
        "judge((clicks>0)).ndcg.@10" -> Right(0.6309297535714574),
        "label(clicks).mrr.@10" -> Right(0.5),
        "label(clicks).ndcg.@10" -> Right(0.6309297535714574),
        "judge((clicks>10))" -> Left(NoValidJudgements),
        "label(clicks).@60" -> Left(KGreaterThanMax), // metric key stops building on error so Errors aren't repeated for all downstream combinations
        "judge((clicks>0)).@60" -> Left(KGreaterThanMax)
      )
    )
  }

  "AtK" should "update K" in {

    val resAt4 = Ranked.of(1, 2, 3, 4, 5).atK(4)

    assert(resAt4.contains(
      Ranked(Ranked.at((1:Rank) -> 1, (2: Rank) -> 2, (3: Rank) -> 3, (4: Rank) -> 4, (5: Rank) -> 5).indexes, 4)
    ))
  }

  "Free Eval" should "evaluate correctly" in {

    val metrics =
      label.from[ResultEngs](cartAdds, clicks, purchases, purchases | clicks) >>> atK(10, 20) >>> eval.ndcg

    val results = EngagedResults(
      NonEmptyList.of(1L, 2L to 10L:_*),
      NonEmptyMap.of(
        1L -> (1.click + 1.cartAdd + 1.purchase),
        2L -> (2.click + 2.cartAdd + 2.purchase),
        3L -> (3.click + 3.cartAdd + 3.purchase),
        4L -> (4.click + 4.cartAdd + 4.purchase)
      )
    )

    val result = metrics.run(results)

    println(result)

    assert(result.lookup("label(cartadd).ndcg.@10").exists(_.contains(0.6020905207089401)))
  }

  "NDCG" should "compute accurately" in {

    val rankedLabels = Ranked.at(
      Rank(1) -> Label(1.0),
      Rank(4) -> Label(1.0),
      Rank(10) -> Label(1.0),
      Rank(25) -> Label(1.0),
      Rank(49) -> Label(1.0),
      Rank(70) -> Label(1.0)
    )

    assert(rankedLabels.ndcgK(50).contains(0.7155165369503295))
  }

  "Free Eval" should "combine" in {
    import eval._

    val results = EngagedResults(
      NonEmptyList.fromListUnsafe((1L to 60L).toList),
      NonEmptyMap.of(
        1L -> (10.clicks + 5.cartAdds + 1.purchase),
        4L -> (20.clicks + 5.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 1.click)
      )
    )

    val metrics =
      label.from[ResultEngs](clicks, cartAdds, purchases) >>>
        atK(10, 20, 30, 40, 50, 60) >++
        (ndcg, precision, recall, rPrecision)

    val result = metrics.run(results)
    assert(result.length == 72)
    assert(result.lookup("label(click).ndcg.@50").exists(_.contains(0.31792843661581627)))
  }


  "Free Eval" should "combine both labelers and judgements" in {
    import eval._

    val results = EngagedResults(
      NonEmptyList.fromListUnsafe((1L to 60L).toList),
      NonEmptyMap.of(
        1L -> (10.clicks + 5.cartAdds + 1.purchase),
        4L -> (20.clicks + 5.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 1.click)
      )
    )

    val labelers = label.from[ResultEngs](clicks, cartAdds, purchases)
    val judgements = judge.from[ResultEngs](anyClicks, anyCartAdds, anyPurchases)

    val metrics =
      (labelers <+> judgements) >>>
        atK(10, 20, 30, 40, 50, 60) >++
        (ndcg, precision, recall, rPrecision)

    val result = metrics.run(results)
    assert(result.length == 144)
  }
}

