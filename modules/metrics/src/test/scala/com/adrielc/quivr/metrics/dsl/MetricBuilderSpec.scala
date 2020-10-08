package com.adrielc.quivr.metrics.dsl

import cats.data.NonEmptyList
import com.adrielc.quivr.free.FreeArrow
import org.scalatest.{FlatSpec, Matchers}
import com.adrielc.quivr.metrics.EngagementCounts
import com.adrielc.quivr.metrics.data.EngagedResults

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
    import FreeArrow._
    import rank._

    val eval =
      plusAll (
        +|(Purchase -> 10.0),
        +|(CartAdd -> 5.0, Purchase -> 10.0),
        +|(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0),
        plusAll(NonEmptyList.of(Click, CartAdd, Purchase).map(e => count(e) <+> binary(count(e))))) >>>
        plusAll(atK(5), atK(10), atK(50), atK(60)) >>>
        (plusAll(NonEmptyList.of(Pow2, Pow1p1, Pow1p01).map(ndcg2(_))) <+> recall <+> precision)

    val f = compileToEvaluator(eval)

    val res = f(EngagedResults(results, engagements))

    assert(res.get("countPurchase.recall.@50").contains(0.8))
  }
}
