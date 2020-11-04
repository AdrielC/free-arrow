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


  "ifThenElse" should "chain logic" in {

    val weighted  = ((anyClicks ->> 1) + (anyCartAdds ->> 5) + (anyPurchases ->> 25)).from[ResultEngs]

    val expected = NonEmptyMap.of(1L -> 31.0, 2L -> 31.0, 3L -> 31.0, 4L -> 31.0, 8L -> 5.0, 9L -> 25.0, 10L -> 1.0)

    assert(weighted.run(results).exists(_.labels == expected))
  }

  "Judgements" should "exclude any result with either only clicks or nothing" in {

    assert((anyCartAdds | anyPurchases).run(results).contains(NonEmptySet.of(1L, 2L, 3L, 4L, 8L, 9L)))
  }
}
