package com.adrielc.quivr.metrics.dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import com.adrielc.quivr.metrics.MyEngagement
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import com.adrielc.quivr.metrics.data.EngagedResults

class MetricBuilderSpec extends FlatSpec with Matchers {
  import MyEngagement._


  type Labs = WithLabels[EngagedResults[MyEngagement]]
  type ResTruth = WithGroundTruth[WithLabels[EngagedResults[MyEngagement]]]
  type Res = EngagedResults[MyEngagement]

  val results = EngagedResults(
    NonEmptyList.fromListUnsafe((1L to 60L).toList),
    NonEmptyMap.of(
      1L -> EngagementCounts(1, 1, 2),
      20L -> EngagementCounts(2, 2, 2),
      30L -> EngagementCounts(3, 3, 3),
      40L -> EngagementCounts(4, 4, 4),
      70L -> EngagementCounts(7, 7, 1)
    )
  )

  "combine" should "build" in {

    val metrics = label.count.from[EngagedResults[MyEngagement]](
      purchases,
      clicks && (purchases.filter === 10) // is not valid, none have 100 clicks therefore should be missing from metrics
    ) >>> atK(50) >>> judge.label.pos >>> (eval.recall[ResTruth] &&& eval.precision)

    val res = metrics.run(results)

    println(res)

    assert(res.length == 2)
    assert(res.lookup("label(purchases).judgeLabel>0.(recall,precision).@50").exists(_.contains((0.8, 0.08))))
  }

  "ranking metrics" should "be applied" in {

    val dups =
      (atK[Res](10, 20) >>> label.count.from[Res](clicks, clicks, clicks) >>> eval.ndcg) <+>
        (atK[Res](30, 40) >>> judge.count.from[Res](judge.count.any[MyEngagement](Click)) >>> eval.averagePrecision)


    val res = dups.run(results)

    println(res)

    assert(res.length == 4)
    assert(res.lookup("judge((clicks>0)).ap.@40").isDefined)
    assert(res.lookup("judge((clicks>0)).ap.@40").exists(_.contains(0.32500000000000007)))
  }
}
