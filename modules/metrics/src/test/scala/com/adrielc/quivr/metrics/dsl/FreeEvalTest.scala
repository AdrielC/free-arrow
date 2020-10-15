package com.adrielc.quivr
package metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data._
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._


class FreeEvalTest extends FlatSpec with Matchers {

  "AtK" should "update K" in {

    val resAt4 = Ranked.of(1, 2, 3, 4, 5).atK(4)

    assert(resAt4.contains(
      Ranked(NonEmptyMap.of((1:Rank) -> 1, (2: Rank) -> 2, (3: Rank) -> 3, (4: Rank) -> 4, (5: Rank) -> 5), 4)
    ))
  }

  "Free Eval" should "evaluate correctly" in {


    val metrics = label.engagement.count(Click, CartAdd) >>> atK(10) >>> eval.rank.ndcg

    val results = EngagedResults(
      NonEmptyList.of(1L, 2L to 10L:_*),
      NonEmptyMap.of(
        1L -> (1.click + 1.cartAdd + 1.purchase),
        2L -> (2.click + 2.cartAdd + 2.purchase),
        3L -> (3.click + 3.cartAdd + 3.purchase),
        4L -> (4.click + 4.cartAdd + 4.purchase)
      )
    )

    val result = metrics.evaluate(results).toMap

    assert(result.get("countCartAdd.ndcg.@10").contains(0.6020905207089401))
  }

  "Free Eval" should "combine" in {

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
      label.engagement.count(Click, Purchase, CartAdd) >>> atK(10, 20, 30, 40, 50, 60) >>> eval.rank.ndcg


    val result = metrics.evaluate(results)

    assert(result.size == 18)
    assert(result.get("countClick.ndcg.@50").contains(0.31792843661581627))
  }

  "Engagements" should "become labels" in {

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


    assertResult(Some(
      NonEmptyMap.of(
        1L -> 6.0,
        4L -> 5.0,
        10L -> 8.0,
        25L -> 15.0,
        49L -> 3.0,
        70L -> 2.0
    ))) {
      val labeler = (Purchase + CartAdd) >>> atK(50)
      labeler.apply(results).map(_.labels)
    }
  }
}

