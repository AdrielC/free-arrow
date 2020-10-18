package com.adrielc.quivr
package metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import com.adrielc.quivr.metrics.data._
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._

object test {
  type ResultEngs = EngagedResults[Engagement]
  val clicks = engagement.count(Engagement.Click: Engagement)
  val cartAdds = engagement.count(Engagement.CartAdd: Engagement)
  val purchases = engagement.count(Engagement.Purchase: Engagement)
}


class FreeEvalTest extends FlatSpec with Matchers {
  import test._

  "AtK" should "update K" in {

    val resAt4 = Ranked.of(1, 2, 3, 4, 5).atK(4)

    assert(resAt4.contains(
      Ranked(NonEmptyMap.of((1:Rank) -> 1, (2: Rank) -> 2, (3: Rank) -> 3, (4: Rank) -> 4, (5: Rank) -> 5), 4)
    ))
  }

  "Free Eval" should "evaluate correctly" in {

    val metrics =
      engagement.toLabel[ResultEngs](cartAdds, clicks, purchases) >>> atK(10) >>> eval.ndcg

    val results = EngagedResults(
      NonEmptyList.of(1L, 2L to 10L:_*),
      Map(
        1L -> (1.click + 1.cartAdd + 1.purchase),
        2L -> (2.click + 2.cartAdd + 2.purchase),
        3L -> (3.click + 3.cartAdd + 3.purchase),
        4L -> (4.click + 4.cartAdd + 4.purchase)
      )
    )

    val result = metrics.evaluate(results)

    println(result)

    assert(result.get("label(cartadds).ndcg.@10").contains(0.6020905207089401))
  }

  "Free Eval" should "combine" in {

    val results = EngagedResults(
      NonEmptyList.fromListUnsafe((1L to 60L).toList),
      Map(
        1L -> (10.clicks + 5.cartAdds + 1.purchase),
        4L -> (20.clicks + 5.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 1.click)
      )
    )

    val metrics = engagement.toLabel[ResultEngs](clicks, cartAdds, purchases) >>> atK(10, 20, 30, 40, 50, 60) >>> eval.ndcg

    val result = metrics.evaluate(results)

    assert(result.size == 18)
    assert(result.get("label(clicks).ndcg.@50").contains(0.31792843661581627))
  }

  "Engagements" should "become labels" in {

    val engagements = Map(
      1L -> (10.clicks + 5.cartAdds + 1.purchase),
      4L -> (20.clicks + 5.cartAdds),
      10L -> (2.purchases + 6.cartAdds + 23.clicks),
      25L -> (5.purchases + 10.cartAdds + 1.click),
      49L -> (3.cartAdds + 6.clicks),
      70L -> (1.purchase + 1.cartAdd + 1.click)
    )


    assertResult(
      NonEmptyMap.of(
        1L -> 6.0,
        4L -> 5.0,
        10L -> 8.0,
        25L -> 15.0,
        49L -> 3.0,
        70L -> 2.0
    )) {
      engagement.toLabel(cartAdds + purchases)
        .evaluate(engagements)
        .values.head.labels
    }
  }


  "Engagements" should "turn into labels" in {

    val engagements =
      Map(
        1L -> (10.clicks + 5.cartAdds + 1.purchase),
        4L -> (20.clicks + 5.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 200.click)
      )

    import engagement._

    val standardWeightedEngs = clicks + (cartAdds * 5) + (purchases * 25)

    val negativeIfTwoTimesMoreClicks = ifThen((((purchases + cartAdds) | 0) <= 2) && (clicks >= 200), -1)

    val a  = negativeIfTwoTimesMoreClicks | ifThen(standardWeightedEngs < 50, 30) | 500

    assertResult(Map(1 -> 500.0, 4 -> 500.0, 10 -> 500.0, 25 -> 500.0, 49 -> 30.0, 70 -> -1.0))(
      engagement.toLabel(a).apply(engagements).get.labels
    )
  }
}

