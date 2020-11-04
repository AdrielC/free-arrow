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
      1L -> (1.click + 1.cartAdd + 2.purchases),
      20L -> (2.clicks + 2.cartAdds + 2.purchases),
      30L -> (3.clicks + 3.cartAdds + 3.purchases),
      40L -> (4.clicks + 4.cartAdds + 4.purchases),
      70L -> (7.clicks + 7.cartAdds + 7.purchases)
    )
  )

  "combine" should "build" in {

    val labeler = label.count.from[Res](
      purchases,
      clicks && (purchases.filter === 100) // is not valid, none have 100 clicks therefore should be missing from metrics
    )

    val metrics = labeler >>> judge.label.isPositive >>> atK(50) >>> (eval.recall[ResTruth] &&& eval.precision)

    val res = metrics.run(results)

    assert(res.length == 2)
    assert(res.lookup("label(purchase).judge(>=0).(recall,prec).@50").exists(_.contains((0.8, 0.08))))
    assert(res.lookup("label((click&filter(purchase=100))").exists(_.isLeft))
  }

  "ranking metrics" should "be applied" in {

    val levels = atK[Res](10, 20) >>> label.count.from[Res](clicks, cartAdds, purchases)  >>> eval.ndcg
    val binary = atK[Res](30, 40) >>> judge.count.from[Res](anyClicks)                    >>> eval.averagePrecision
    val dups = levels <+> binary

    val res = dups.run(results)

    println(res)

    assert(res.toSortedMap == Map(
      "judge((click>0)).ap.@30"   -> Right(0.4000000000000001),
      "judge((click>0)).ap.@40"   -> Right(0.32500000000000007),
      "label(cartadd).ndcg.@10"   -> Right(1.0),
      "label(cartadd).ndcg.@20"   -> Right(0.46352060224668756),
      "label(click).ndcg.@10"     -> Right(1.0),
      "label(click).ndcg.@20"     -> Right(0.46352060224668756),
      "label(purchase).ndcg.@10"  -> Right(1.0),
      "label(purchase).ndcg.@20"  -> Right(0.7527425666302089)
    ))
    assert(res.lookup("judge((click>0)).ap.@40").isDefined)
    assert(res.lookup("judge((click>0)).ap.@40").exists(_.contains(0.32500000000000007)))
  }
}
