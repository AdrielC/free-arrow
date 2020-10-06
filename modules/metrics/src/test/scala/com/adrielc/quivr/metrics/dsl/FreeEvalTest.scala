package com.adrielc.quivr
package metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data.LabelledIndexes
import com.adrielc.quivr.metrics.dsl.EvalOp.AtK
import org.scalatest.{FlatSpec, Matchers}


class FreeEvalTest extends FlatSpec with Matchers {

  val results = NonEmptyList.fromListUnsafe((1L to 10L).toList)

  val engagements = NonEmptyMap.of(
    1L -> EngagementCounts(1, 1, 1),
    2L -> EngagementCounts(2, 2, 2),
    3L -> EngagementCounts(3, 3, 3),
    4L -> EngagementCounts(4, 4, 4)
  )

  val labelled = LabelledIndexes.of(10)(
    1 -> 1.0,
    2 -> 2.0,
    3 -> 3.0,
    4 -> 4.0,
    100 -> 100
  )


  "AtK" should "filter" in {

    assertResult(Some(LabelledIndexes(NonEmptyMap.of(
      1 -> 1.0,
      2 -> 2.0,
      3 -> 3.0,
      4 -> 4.0,
      100 -> 100.0
    ), 10)))(

      AtK[LabelledIndexes](10).apply(labelled)
    )
  }

  "Free Eval" should "evaluate correctly" in {

    val metrics = {
      import rank._
      for {
        k <- 1 to 100
        e <- List(click, cartAdd, purchase)
        m <- List(ndcg, precision, recall)
      } yield +e >>> atK(k) >>> m
    }

    val f = compileMetrics(metrics, compileToList)

    val result = f((results, engagements)).toMap

    assert(result.size == 90)

    assert(result.get("ndcg.countOfCartAdd.@10").contains(0.6020905207089401))
  }

  "Free Eval" should "combine" in {
    import rank._

    val results = NonEmptyList.fromListUnsafe((1L to 60L).toList)

    val engagements = NonEmptyMap.of(
      1L -> (10.clicks + 5.cartAdds + 1.purchase),
      4L -> (20.clicks + 5.cartAdds),
      10L -> (2.purchases + 6.cartAdds + 23.clicks),
      25L -> (5.purchases + 10.cartAdds + 1.click),
      49L -> (3.cartAdds + 6.clicks),
      70L -> (1.purchase + 1.cartAdd + 1.click)
    )

    val metrics = for {
      k <- 10 to 60 by 10
      e <- List(click, purchase, cartAdd)
      p <- List(pow2, pow1p1, pow1p01)
    } yield +e >>> p >>> atK(k) >>> ndcg

    val f = compileMetrics(metrics, compileToList)

    val result = f((results, engagements)).toMap

    assert(result.size == 54)
    assert(result.get("ndcg.pow2.countClick.@50").contains(0.31792843661581627))
  }
}

