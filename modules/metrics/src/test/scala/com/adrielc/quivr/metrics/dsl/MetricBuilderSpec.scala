package com.adrielc.quivr.metrics.dsl

import cats.arrow.Arrow
import cats.data.{Kleisli, NonEmptyList, NonEmptyMap}
import com.adrielc.quivr.free.{>>>, FreeArrow}
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.quivr.metrics.{EngagedResults, EngagementCounts}

class MetricBuilderSpec extends FlatSpec with Matchers {

  type FAList[A, B] = A >>> List[B]
  type KList[A, B] = Kleisli[List, A, B]
  type ToList[A, B] = A => List[B]
  type FAKleiz[A, B] = FreeArrow[Arrow, KList, A, (String, B)]

  val results = NonEmptyList.fromListUnsafe((1L to 60L).toList)

  val engagements = NonEmptyMap.of(
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
      <+> (
        +|(Purchase -> 10.0),
        +|(CartAdd -> 5.0, Purchase -> 10.0),
        +|(Click -> 1.0, CartAdd -> 5.0, Purchase -> 25.0),
        <+>(NonEmptyList.of(Click, CartAdd, Purchase).map(e => count(e) <+> binary(count(e))))
      ) >>>
        <+>(atK(5), atK(10), atK(50), atK(60)
      ) >>>
        ((<+>(p2, p11, p101) >>> ndcg) <+> (recall <+> precision))

    val f = compileToEvaluator(eval)

    val res = f(EngagedResults(results, engagements))

    assert(res.get("countPurchase.recall.@50").contains(0.8))
  }
}
