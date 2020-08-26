package com.adrielc.arrow.metrics

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._

object evaluables {

  case class LabelledIndexes private (results: NonEmptyMap[Index, Label])
  object LabelledIndexes {

    def apply(h: (Index, Label), t: (Index, Label)*): LabelledIndexes = new LabelledIndexes(NonEmptyMap.of(h, t:_*))
  }

  case class RankingWithRelevants(results: NonEmptySet[ResultId], relevant: NonEmptySet[ResultId])

  case class Page(results: NonEmptySet[ResultId], resultEngagements: ResultEngagements)
}
