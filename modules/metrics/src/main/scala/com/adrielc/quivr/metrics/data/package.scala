package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.auto._

package object data {

  type Rank             = PosInt
  object Rank {
    def apply(c: Int): Either[String, Rank] = PosInt.from(c)
    private[metrics] def fromIndex(c: Int): Rank = PosInt.unsafeFrom(c + 1)
    private[metrics] def unsafe(c: Int): Rank = PosInt.unsafeFrom(c)
  }
  type NonZeroCount = PosInt
  object NonZeroCount {
    def apply(c: Int): Either[String, NonZeroCount] = PosInt.from(c)
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
}
