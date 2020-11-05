package com.adrielc.quivr.metrics

import cats.data.NonEmptyList
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.types.numeric.{NonNegDouble, PosInt}


package object data {

  type ResultId         = Long
  type ResultSet        = NonEmptyList[ResultId]

  type Rank = PosInt
  object Rank extends RefinedTypeOps.Numeric[PosInt, Int] {
    private[metrics] def fromIndex(c: Int): Rank = PosInt.unsafeFrom(c + 1)
  }

  type Label = NonNegDouble
  object Label extends RefinedTypeOps.Numeric[NonNegDouble, Double]

  type Gain = NonNegDouble
  object Gain extends RefinedTypeOps.Numeric[NonNegDouble, Double] {
    val one: Gain = NonNegDouble.unsafeFrom(1.0)
    val zero: Gain = NonNegDouble.unsafeFrom(0.0)
  }

  type NonZeroCount = PosInt
  object NonZeroCount extends RefinedTypeOps.Numeric[PosInt, Int]
}
