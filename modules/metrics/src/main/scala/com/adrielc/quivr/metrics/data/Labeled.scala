package com.adrielc.quivr.metrics.data

import cats.data.{NonEmptyMap, NonEmptySet}
import com.adrielc.quivr.metrics.ResultId

sealed trait Labeled[+A]
object Labeled {
  case class WithGroundTruth[+A](a: A, groundTruthSet: NonEmptySet[ResultId]) extends Labeled[A]
  case class WithResultLabels[+A](a: A, resultLabels: NonEmptyMap[ResultId, Double]) extends Labeled[A]
}
