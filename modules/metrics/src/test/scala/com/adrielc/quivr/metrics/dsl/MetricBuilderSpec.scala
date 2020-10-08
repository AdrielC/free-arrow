package com.adrielc.quivr.metrics.dsl

import cats.data.NonEmptyList
import com.adrielc.quivr.free.FreeArrow
import org.scalatest.{FlatSpec, Matchers}
import com.adrielc.quivr.metrics.EngagementCounts
import com.adrielc.quivr.metrics.data.EngagedResults
import FreeArrow._
import com.adrielc.quivr.metrics.dsl.EvalOp.Metric.Gain
import rank._

class MetricBuilderSpec extends FlatSpec with Matchers {

  val results = NonEmptyList.fromListUnsafe((1L to 60L).toList)

  val engagements = Map(
    1L -> EngagementCounts(1, 1, 1),
    20L -> EngagementCounts(2, 2, 2),
    30L -> EngagementCounts(3, 3, 3),
    40L -> EngagementCounts(4, 4, 4),
    70L -> EngagementCounts(7, 7, 7)
  )

  "combine" should "build" in {

    val eval =
      plusAll (
        +|(Purchase -> 10.0),
        +|(CartAdd -> 5.0, Purchase -> 10.0),
        +|(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0),
        plusAll(NonEmptyList.of(Click, CartAdd, Purchase).map(e => +e <+> !e))) >>>
        plusAll(atK(5), atK(10), atK(50), atK(60)) >>>
        (plusAll(NonEmptyList.of(Gain.Pow2, Gain.Pow1p1, Gain.Pow1p01).map(ndcgWithGain(_))) <+> recall <+> precision)

    val f = compileToEvaluator(eval)

    val res = f(EngagedResults(results, engagements))

    println(res)

    assert(res.get("countPurchase.recall.@50").contains(0.8))
  }

  "ranking metrics" should "be applied" in {

    import com.adrielc.quivr.metrics.dsl.EvalOp.Metric._

    val m = (!CartAdd + !Click)
      .at(10)
      .evalRanking(AveragePrecision)

    val f = compileToEvaluator(m)

    val res = f(EngagedResults(results, engagements))

    println(res)

    assert(res.size == 1)
  }
}
