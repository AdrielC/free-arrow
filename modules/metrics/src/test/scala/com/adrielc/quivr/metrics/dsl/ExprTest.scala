package com.adrielc.quivr.metrics
package dsl

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.quivr.metrics.data.EngagedResults
import implicits._

class ExprTest extends FlatSpec with Matchers {
  import com.adrielc.quivr.metrics.MyEngagement._

  val results = EngagedResults(
    NonEmptyVector.of(1L, 2L to 10L:_*),
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

    val r = labeler.labelResults(results).exists(_.res.toNem.lookup(1L).exists(_.gainOrZero == 3.0))

    assert(r)
  }

  "Labeler" should "sum weighted engagements" in {

    val labeler = label.weightedSum(clicks -> 1, cartAdds -> 5, purchases -> 25)

    val labeler2 = clicks + cartAdds*5 + purchases*25

    val r = labeler.labelResults(results).exists(_.res.toNem.lookup(1L).exists(_.gainOrZero == 31.0))

    assert(r)
    assert(labeler.labelResults(results) == labeler2.labelResults(results))
  }


  "ifThenElse" should "chain logic" in {

    val weighted  = ((anyClicks ->> 1) + (anyCartAdds ->> 5) + (anyPurchases ->> 25)).from[ResultEngs]

    val expected = NonEmptyList.of(1L -> 31.0, 2L -> 31.0, 3L -> 31.0, 4L -> 31.0, 8L -> 5.0, 9L -> 25.0, 10L -> 1.0)

    assert(weighted.run(results).map(_.res.toNem.map(_.gain).toNel.toList.mapFilter(a => a._2.map(a._1 -> _)).toNel.get).right.get == expected)
  }

  "Judgements" should "exclude any result with either only clicks or nothing" in {

    assert((cartAdds | purchases).labelResults(results).flatMap(_.res.toList.filter(_._2.isJudged).toNel.map(_.toNem.keys))
      .contains(NonEmptySet.of(1L, 2L, 3L, 4L, 8L, 9L)))

    assert((anyCartAdds | anyPurchases).run(results).flatMap(_.res.toList.filter(_._2.isJudged).toNel.map(_.toNem.keys))
      .contains(NonEmptySet.of(1L, 2L, 3L, 4L, 8L, 9L, 10L)))
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


    assert((cartAdds + purchases).engsToLabels(engagements) == Map(
      1L -> 6.0.some,
      4L -> 5.0.some,
      10L -> 8.0.some,
      25L -> 15.0.some,
      49L -> 3.0.some,
      69L -> None,
      70L -> 2.0.some
    ))
  }


  "Engagements" should "turn into labels" in {

    val engagements =
      NonEmptyMap.of(
        1L  -> (10.clicks + 5.cartAdds + 1.purchase),
        4L  -> (20.clicks + 6.cartAdds),
        10L -> (2.purchases + 6.cartAdds + 23.clicks),
        25L -> (5.purchases + 10.cartAdds + 1.click),
        49L -> (3.cartAdds + 6.clicks),
        70L -> (1.purchase + 1.cartAdd + 200.click)
      )

    val res = EngagedResults(NonEmptyVector.of(1L, 2L to 70L:_*), engagements)

    val standardWeightedEngs = clicks + cartAdds*5 + purchases*25

    val negativeIfTwoTimesMoreClicks = (((purchases + cartAdds) <= 2) && (clicks >= 200)) ->> -1

    val a  = negativeIfTwoTimesMoreClicks | ((standardWeightedEngs < 50) ->> 30) | 500

    val evaluator = a.from[ResultEngs] >>> eval.ndcg

    assert(evaluator.run(res) == Right(0.7544045426339389))

    assert(a.labelResults(res).foldMapK(_.res.toNem.toSortedMap.toList.mapFilter{case (k, v) => v.gain.map(k -> _)}).toMap == Map(
      1L -> 500.0,
      4L -> 500.0,
      10L -> 500.0,
      25L -> 500.0,
      49L -> 30.0,
      70L -> -1.0
    ))
  }
}
