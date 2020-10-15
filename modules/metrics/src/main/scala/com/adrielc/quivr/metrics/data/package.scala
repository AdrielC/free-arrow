package com.adrielc.quivr.metrics

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.data.Judged.{WithLabels, WithGroundTruth}
import eu.timepit.refined.types.numeric.{PosInt, PosLong}

package object data {

  type Rank           = PosInt
  type NonZeroCount   = PosLong
  type ResultId       = Long
  type Label          = Double
  type Relevance      = Boolean

  type LabelsAtK      = NonEmptyList[Label]
  type RelevanceAtK   = NonEmptyList[Relevance]
  type SetRelevance   = WithGroundTruth[NonEmptyList[ResultId]]
  type SetLabels      = WithLabels[NonEmptyList[ResultId]]
}
