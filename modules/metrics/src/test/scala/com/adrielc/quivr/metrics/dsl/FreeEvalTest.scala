package com.adrielc.quivr
package metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{EngagedResults, LabelledIndexes}
import org.scalatest.{FlatSpec, Matchers}


class FreeEvalTest extends FlatSpec with Matchers {

  "AtK" should "filter" in {

    assertResult(Some(LabelledIndexes(NonEmptyMap.of(
      1 -> 1.0,
      2 -> 2.0,
      3 -> 3.0,
      4 -> 4.0,
      100 -> 100.0
    ), 10)))(

      LabelledIndexes.of(1 -> 1.0, 2 -> 2.0, 3 -> 3.0, 4 -> 4.0, 100 -> 100).toK(10)
    )
  }

  "Free Eval" should "evaluate correctly" in {

    import rank._
    import com.adrielc.quivr.free.FreeArrow._

    val metrics =
      plusAll(+Click, +CartAdd, +Purchase)
        .at(1, 2 to 100:_*) >>>
        plusAll(ndcg, precision, recall)

    val f = compileToEvaluator(metrics)

    val results = NonEmptyList.of(1L, 2L to 10L:_*)

    val engagements = Map(
      1L -> EngagementCounts(1, 1, 1),
      2L -> EngagementCounts(2, 2, 2),
      3L -> EngagementCounts(3, 3, 3),
      4L -> EngagementCounts(4, 4, 4)
    )

    val result = f(EngagedResults(results, engagements))

    assert(result.size == 90)

    assert(result.get("countCartAdd.ndcg.@10").contains(0.6020905207089401))
  }

  "Free Eval" should "combine" in {
    import rank._
    import com.adrielc.quivr.free.FreeArrow.plusAll

    val results = NonEmptyList.fromListUnsafe((1L to 60L).toList)

    val engagements = Map(
      1L -> (10.clicks + 5.cartAdds + 1.purchase),
      4L -> (20.clicks + 5.cartAdds),
      10L -> (2.purchases + 6.cartAdds + 23.clicks),
      25L -> (5.purchases + 10.cartAdds + 1.click),
      49L -> (3.cartAdds + 6.clicks),
      70L -> (1.purchase + 1.cartAdd + 1.click)
    )

    val metrics =
      plusAll(+Click, +Purchase, +CartAdd) >>>
        plusAll(NonEmptyList.of(10, 20 to 60 by 10:_*).map(atK[LabelledIndexes](_))) >>>
        plusAll(NonEmptyList.of(Pow2, Pow1p1, Pow1p01).map(ndcgWithGain(_)))

    val f = compileToEvaluator(metrics)

    val result = f(EngagedResults(results, engagements))

    assert(result.size == 54)
    assert(result.get("countClick.ndcg.@50").contains(0.31792842410318195))
  }
}

