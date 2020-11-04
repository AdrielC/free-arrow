package com.adrielc.quivr.metrics.dsl

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import com.adrielc.quivr.metrics.data.EngagedResults
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class ExprTest extends FlatSpec with Matchers {
  import com.adrielc.quivr.metrics.MyEngagement._

  val results = EngagedResults(
    NonEmptyList.of(1L, 2L to 10L:_*),
    NonEmptyMap.of(
      1L -> (1.click + 1.cartAdd + 1.purchase),
      2L -> (2.click + 2.cartAdd + 2.purchase),
      3L -> (3.click + 3.cartAdd + 3.purchase),
      4L -> (4.click + 4.cartAdd + 4.purchase),
      8L -> 1.cartAdd,
      9L -> 1.purchase,
      10L -> 1.click
    )
  )

  "Labeler" should "sum engagements" in {

    val labeler = clicks + cartAdds + purchases

    assert(labeler.run(results).get(1L).contains(3.0))
  }

  "Labeler" should "sum weighted engagements" in {

    val labeler = label.count.weightedSum(clicks -> 1, cartAdds -> 5, purchases -> 25)

    val labeler2 = clicks + cartAdds*5 + purchases*25

    assert(labeler.run(results).get(1L).contains(31.0))

    assert(labeler.run(results).get(1L) == labeler2.run(results).get(1L))
  }


  "ifThenElse" should "chain logic" in {

    val weighted  = ((anyClicks ->> 1) + (anyCartAdds ->> 5) + (anyPurchases ->> 25)).from[ResultEngs]

    val expected = NonEmptyMap.of(1L -> 31.0, 2L -> 31.0, 3L -> 31.0, 4L -> 31.0, 8L -> 5.0, 9L -> 25.0, 10L -> 1.0)

    assert(weighted.run(results).exists(_.labels == expected))
  }

  "Judgements" should "exclude any result with either only clicks or nothing" in {

    assert((anyCartAdds | anyPurchases).run(results).contains(NonEmptySet.of(1L, 2L, 3L, 4L, 8L, 9L)))
  }

  "Engagements" should "become labels" in {

    val engagements = Map(
      1L  -> (10.clicks + 5.cartAdds + 1.purchase),
      4L  -> (20.clicks + 5.cartAdds),
      10L -> (2.purchases + 6.cartAdds + 23.clicks),
      25L -> (5.purchases + 10.cartAdds + 1.click),
      49L -> (3.cartAdds + 6.clicks),
      69L -> 1.click,
      70L -> (1.purchase + 1.cartAdd + 1.click)
    ).mapValues(_.toMap)


    assert((cartAdds + purchases).run(engagements) == Map(
      1L -> 6.0,
      4L -> 5.0,
      10L -> 8.0,
      25L -> 15.0,
      49L -> 3.0,
      70L -> 2.0
    ))
  }


  "Engagements" should "turn into labels" in {

    val engagements =
      Map(
        1L  -> (10.clicks + 5.cartAdds + 1.purchase),
        4L  -> (20.clicks + 6.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 200.click)
      ).mapValues(_.toMap)

    val standardWeightedEngs = clicks + (cartAdds * 5) + (purchases * 25)

    val negativeIfTwoTimesMoreClicks = (((purchases + cartAdds) <= 2) && (clicks >= 200)) ->> -1

    val a  = negativeIfTwoTimesMoreClicks | ((standardWeightedEngs < 50) ->> 30) | 500

    assert(a.run(engagements) == Map(
      1L -> 500.0,
      4L -> 500.0,
      10L -> 500.0,
      25L -> 500.0,
      49L -> 30.0,
      70L -> -1.0
    ))
  }
}
