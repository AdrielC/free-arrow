package com.adrielc.quivr.metrics.dsl

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.data.Engagement
//import com.adrielc.quivr.free.FreeArrow
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
//import FreeArrow._
//import com.adrielc.quivr.metrics.data.Judged.WithGroundTruth
import com.adrielc.quivr.metrics.data.{EngagedResults, EngagementCounts}

class MetricBuilderSpec extends FlatSpec with Matchers {
  import test._

  val results = EngagedResults(
    NonEmptyList.fromListUnsafe((1L to 60L).toList),
    Map(
      1L -> EngagementCounts(1L, 1L, 2L),
      20L -> EngagementCounts(2L, 2L, 2L),
      30L -> EngagementCounts(3L, 3L, 3L),
      40L -> EngagementCounts(4L, 4L, 4L),
      70L -> EngagementCounts(7L, 7L, 1L)
    )
  )

  "combine" should "build" in {

    val metrics = engagement.toJudgement[EngagedResults[Engagement]](
      purchases,
      clicks && (purchases === 10) // is not valid, none have 100 clicks therefore should be missing from metrics
    ) >>> atK(50) >>> eval.recall

    val res = metrics.evaluate(results)

    assert(res.size == 1)
    assert(res.get("judge(purchases).recall.@50").contains(0.8))
  }

//  "ranking metrics" should "be applied" in {
//
//    def evaluate[A] = atK[SetLabels](10, 20) >>>
//      judge.label.isPositive[SetLabels] >>>
//      eval.judge.averagePrecision
//
//    val wonky = label.engagement.sum(Click).merge(label.engagement.sum(CartAdd)) >>> (evaluate *** evaluate)
//
//    val _ = wonky.tapOut(println).evaluate(results)
//
//    val res = wonky.evaluate(results)
//
//    println(res)
//
//    assert(res.size == 1)
//  }
}
