package com.adrielc.quivr.metrics.dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import com.adrielc.quivr.free.FreeArrow
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import FreeArrow._
import com.adrielc.quivr.metrics.data.SetLabels
import com.adrielc.quivr.metrics.{EngagedResults, EngagementCounts}

class MetricBuilderSpec extends FlatSpec with Matchers {

  val results = EngagedResults(
    NonEmptyList.fromListUnsafe((1L to 60L).toList),
    NonEmptyMap.of(
      1L -> EngagementCounts(1L, 1L, 1L),
      20L -> EngagementCounts(2L, 2L, 2L),
      30L -> EngagementCounts(3L, 3L, 3L),
      40L -> EngagementCounts(4L, 4L, 4L),
      70L -> EngagementCounts(7L, 7L, 7L)
    )
  )

  "combine" should "build" in {

    val metrics =
      label.together(
        label.engagement.count.weighted(Purchase -> 10.0),
        label.engagement.count.weighted(CartAdd -> 5.0, Purchase -> 10.0),
        label.engagement.count.weighted(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0),
        label.engagement.count(Click, CartAdd, Purchase)) >>>
        judge.label.isPositive >>>
        atK(5, 10, 50, 70) >>>
        eval.together (
          eval.rank.ndcg,
          eval.retrieval.precision,
          eval.retrieval.recall
        )

    val res = metrics.evaluate(results)

    println(res)

    assert(res.get("countPurchase.relevance>0.recall.@50").contains(0.8))
  }

  "ranking metrics" should "be applied" in {

    val wonky = Click.count.merge(CartAdd.count)._2 >>>
      atK(10, 20) >>>
      judge.label.isPositive[SetLabels] >>>
      eval.rank.averagePrecision

    val m = CartAdd.count >>> judge.label.isPositive >>> atK(70, 1000) >>> eval.rank.averagePrecision

    val _ = m.tapOut(println).evaluate(results)

    val res = wonky.evaluate(results)


    println(res)

    assert(res.size == 1)
  }
}
