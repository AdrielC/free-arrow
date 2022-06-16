package com.adrielc.quivr
package metrics
package dsl

import cats.{Contravariant, Order}
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.metrics.ranking.ResultRelevancies

import scala.collection.immutable.SortedMap
import cats.implicits._
import com.adrielc.quivr.metrics.MyEngagement.ResultEngs
import com.adrielc.quivr.metrics.data._
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import MyEngagement._
import com.adrielc.quivr.metrics.dsl.evaluation.EvalError.KGreaterThanMax
import metrics.implicits._
import Rankings.Ranked


class FreeEvalTest extends FlatSpec with Matchers {

  "Eval" should "run getting started" in {

    case object Clicks // user defined engagement domain

    implicit val order: Order[Clicks.type] = Order.allEqual[Clicks.type]

    type Results = (NonEmptyList[ResultId], Map[ResultId, Map[Clicks.type , Int]]) // user defined results type

    implicit val c = Contravariant[ResultRelevancies].contramap(
      ResultRelevancies[EngagedResults[Clicks.type ]])((a: Results) =>
      EngagedResults(a._1.toNev, NonEmptyMap.fromMap(SortedMap(a._2.mapValues(
        KeyCounts.fromMap(_).get)
        .toList:_*)).get))

    val results: Results = (
      NonEmptyList.of(1L, 2L to 59L:_*), // results
      Map(2L -> Map(Clicks -> 1))               // engagement counts
    )

    val evaluation = label.of (Clicks) .from [Results] >>> atK(10, 60) >>> eval.ndcg

    val metrics = evaluation.run(results).toSortedMap

    assert(metrics ==
      Map(
        "clicks.mrr.@10" -> Right(0.5),
        "clicks.ndcg.@10" -> Right(0.6309297535714574),
        "clicks.@60" -> Left(KGreaterThanMax(60)) // metric key stops building on error so Errors aren't repeated for all downstream combinations
      )
    )
  }

  "AtK" should "update K" in {

    val resAt4 = Ranked.of(1, 2, 3, 4, 5).atK(4)

    assert(resAt4.contains(
      Ranked(
        Ranked.at(
          (1:Rank) -> 1,
          (2: Rank) -> 2,
          (3: Rank) -> 3,
          (4: Rank) -> 4,
          (5: Rank) -> 5)
          .rankings,
        4)
    ))
  }

  "Free Eval" should "evaluate correctly" in {

    val results = EngagedResults(
      NonEmptyVector.of(1L, 2L to 10L:_*),
      NonEmptyMap.of(
        1L -> (1.click + 1.cartAdd + 1.purchase),
        2L -> (2.click + 2.cartAdd + 2.purchase),
        3L -> (3.click + 3.cartAdd + 3.purchase),
        4L -> (4.click + 4.cartAdd + 4.purchase)
      )
    )

    val metrics =
      label.from [ResultEngs] (cartAdds, clicks, purchases, purchases or clicks) >>>
        atK(10, 20) >>>
        eval.ndcg

    val result = metrics.run(results)

    assert(result.lookup("cartadd.ndcg.@10").exists(_.contains(0.6020905207089401)))
  }

  "Free Eval" should "combine" in {
    import eval._

    val results = EngagedResults(
      NonEmptyVector.of(1L, 2L to 60L:_*),
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
      label.from[ResultEngs](
        clicks, cartAdds, purchases) >>>
        atK(10, 20, 30, 40, 50, 60) >>>
        eval(ndcg, precision, recall, rPrecision)

    val result = metrics.run(results)
    assert(result.length == 72)
    assert(result.lookup("click.ndcg.@50").exists(_.contains(0.31792843661581627)))
  }


  "Free Eval" should "combine both labelers and judgements" in {
    import eval._

    val results = EngagedResults(
      NonEmptyVector.of(1L, 2L to 60L:_*),
      NonEmptyMap.of(
        1L -> (10.clicks + 5.cartAdds + 1.purchase),
        4L -> (20.clicks + 5.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 1.click)
      )
    )

    val metrics = ^[ResultEngs] >>> (
      (label.from[ResultEngs](clicks, cartAdds, purchases).covaryOut[Labeled[ResultEngs]] <+>
      judge.from[ResultEngs](anyClicks, anyCartAdds, anyPurchases).covaryOut[Labeled[ResultEngs]])) >>>
      atK(10, 20, 30, 40, 50, 60) >>>
      eval(ndcg, precision, recall, rPrecision)

    val result = metrics.run(results)
    assert(result.length == 144)
  }

  private val results = EngagedResults(
    NonEmptyVector.of(1L, 2L to 60L:_*),
    NonEmptyMap.of(
      1L -> (10.clicks + 5.cartAdds + 1.purchase),
      4L -> (20.clicks + 5.cartAdds),
      10L -> (2.purchases + 6.cartAdds + 23.clicks),
      25L -> (5.purchases + 10.cartAdds),
      49L -> (3.cartAdds + 6.clicks),
      70L -> (1.purchase + 1.cartAdd + 1.click)
    )
  )

  "Filter" should "filter out result sets" in {

    val evaluation = label.from [ResultEngs] (clicks) >>> atK (60) >>> eval.qMeasure (1)

    assert(evaluation.run(results)._2.contains(0.9317013470520544))
  }

  "Results" should " should count events properly" in {

    val evaluation = (clicks > 5).run(results)

    print(evaluation.get.labels)
  }
}

