package com.adrielc.quivr

import cats.data.NonEmptyMap
import com.adrielc.quivr.data.~>|
import com.adrielc.quivr.free.FA
import com.adrielc.quivr.metrics.EngagementType.{CartAdd, Click, Purchase}

import scala.math.{log, pow}

package object metrics extends ToK.ToToKOps {

  type Evaluator[A, B] = FA[EvalOp, A, B]
  type Labeler[A, B] = FA[LabelOp, A, B]

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
    private def guard[N](n: N)(e: EngagementType)(implicit N: Numeric[N]): EngagementCounts = if(N.gt(n, N.zero)) Map(e -> N.toLong(n)) else Map.empty
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

  private[metrics] val log2 = (i: Double) => log(i) / log(2)
  private[metrics] val pow2 = (i: Double) => pow(2, i) - 1.0

  private[metrics] def collectOps[F[_,_]]: F ~>| List[F[_, _]] = new (F ~>| List[F[_, _]]) {
    def apply[A, B](fab: F[A, B]): List[F[_, _]] = List(fab)
  }
}
