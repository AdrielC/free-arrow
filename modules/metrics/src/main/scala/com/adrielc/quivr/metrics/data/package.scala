package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import eu.timepit.refined.types.numeric.{PosInt, PosLong}

package object data {

  type Rank             = PosInt
  type NonZeroCount     = PosLong
  object NonZeroCount {
    def apply(c: Long): Either[String, NonZeroCount] = PosLong.from(c)
  }
  type ResultId         = Long
  type Label            = Double
  type Relevance        = Boolean

  type TruthSet         = NonEmptySet[ResultId]
  type Labels           = NonEmptyMap[ResultId, Label]

  type ResultSet        = NonEmptyList[ResultId]
  type LabelledResults  = NonEmptyList[Label]
  type JudgedResults    = NonEmptyList[Relevance]

  type SetRelevance     = WithGroundTruth[ResultSet]
  type SetLabels        = WithLabels[ResultSet]
  type EngagementCounts = KeyCounts[Engagement]
  object EngagementCounts {

    def apply(click: NonZeroCount, cart: NonZeroCount, purchase: NonZeroCount): EngagementCounts =
      clicks(click) + cartAdds(cart) + purchases(purchase)

    def clicks(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(Engagement.Click, n))

    def cartAdds(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(Engagement.CartAdd, n))

    def purchases(n: NonZeroCount): EngagementCounts =
      KeyCounts(NonEmptyMap.one(Engagement.Purchase, n))
  }
}
