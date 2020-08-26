package com.adrielc.arrow

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._
import com.adrielc.arrow.metrics.EvalOp.EngagementType.{CartAdd, Click, Purchase}
import com.adrielc.arrow.metrics.EvalOp.{EngagementCounts, EngagementType}

package object metrics {

  type Index = Int
  type Label = Double
  type ResultId = Long
  type EngagementWeights = Map[EngagementType, Double]
  type ResultEngagements = Map[ResultId, EngagementCounts]

  implicit class EngOps[N](private val n: N) extends AnyVal {
    def clicks(implicit N: Numeric[N]): EngagementCounts = EngagementCounts(Map(Click -> N.toLong(n)))
    def cartAdds(implicit N: Numeric[N]): EngagementCounts = EngagementCounts(Map(CartAdd -> N.toLong(n)))
    def purchases(implicit N: Numeric[N]): EngagementCounts = EngagementCounts(Map(Purchase -> N.toLong(n)))
  }

  implicit class RichNumeric[N](n: N)(implicit num: Numeric[N]) {
    def binarize: N = if(num.compare(n, num.zero) > 0) num.one else num.zero
  }
}
