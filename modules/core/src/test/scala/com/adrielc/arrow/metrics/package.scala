package com.adrielc.arrow

import cats.data.NonEmptyMap
import com.adrielc.arrow.free.FA
import com.adrielc.arrow.metrics.EngagementType.{CartAdd, Click, Purchase}

import scala.math.Numeric.Implicits._

package object metrics extends ToK.ToToKOps {

  type FreeEval[A, B] = FA[EvalOp, A, B]
  type FreeLabel[A, B] = FA[LabelOp, A, B]

  type Index = Int
  type Label = Double
  type Count = Long
  type ResultId = Long
  type EngagementWeights = Map[EngagementType, Double]
  type EngagedResults = Map[ResultId, EngagementCounts]
  type LabelledResults = NonEmptyMap[ResultId, Label]

  type EngagementCounts = Map[EngagementType, Count]
  object EngagementCounts {
    def apply(click: Int = 0, cartAdd: Int = 0, purchase: Int = 0): EngagementCounts = clicks(click) ++ cartAdds(cartAdd) ++ purchases(purchase)
    def clicks[N : Numeric](n: N): EngagementCounts = guard(n)(Click)
    def cartAdds[N : Numeric](n: N): EngagementCounts = guard(n)(CartAdd)
    def purchases[N : Numeric](n: N): EngagementCounts = guard(n)(Purchase)
    private def guard[N](n: N)(e: EngagementType)(implicit N: Numeric[N]): EngagementCounts = if(N.gt(n, N.zero)) Map(e -> n.toLong) else Map.empty
  }

  implicit class EngCountOps[N](private val n: N) extends AnyVal {
    def clicks(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.clicks(n)
    def cartAdds(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.cartAdds(n)
    def purchases(implicit N: Numeric[N]): EngagementCounts = EngagementCounts.purchases(n)
  }

  implicit class RichNumeric[N](n: N)(implicit N: Numeric[N]) {
    def binarize: N = if(N.gt(n, N.zero)) N.one else N.zero
  }

  implicit class EngOps(private val engs: EngagementCounts) extends AnyVal {
    def countOf(e: EngagementType): Long = engs.getOrElse(e, 0)
    def clicks   : Long = countOf(Click)
    def cartAdds : Long = countOf(CartAdd)
    def purchases: Long = countOf(Purchase)
  }
}
