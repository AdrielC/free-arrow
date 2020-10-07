package com.adrielc.quivr

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import com.adrielc.quivr.metrics.data.{IndexedResults, WithRelevant}
import cats.implicits._
import com.adrielc.quivr.metrics.dsl.Engagement

import scala.math.{log, pow}
import scala.util.Try

package object metrics
  extends ToK.ToToKOps
  with ResultsCount.ToResultsCountOps
  with PartialResultSet.ToPartialResultSetOps
  with RelevantCount.ToRelevantCountOps
  with ResultSet.ToResultSetOps
  with RelevantResultSet.ToRelevantResultSetOps
  with IndexedLabels.ToIndexedLabelsOps {

  type ResultsWithEngagements = IndexedResults[Option[EngagementCounts]]

  type ResultsWithRelevancy[+A] = WithRelevant[IndexedResults[A]]

  type Index        = Int
  type Count        = Long
  type Label        = Double
  type ResultId     = Long
  type Query        = String
  type Labels       = NonEmptyMap[Index, Label]
  type Results      = NonEmptyMap[Index, ResultId]
  type Relevant     = NonEmptySet[ResultId]

  type EngagementWeights  = Map[Engagement, Double]
  type Engagements        = NonEmptyMap[ResultId, EngagementCounts]

  type EngagementCounts = Map[Engagement, Count]
  object EngagementCounts {
    def apply(click: Int = 0, cartAdd: Int = 0, purchase: Int = 0): EngagementCounts = clicks(click) ++ cartAdds(cartAdd) ++ purchases(purchase)
    def clicks[N : Numeric](n: N): EngagementCounts = guard(n)(Engagement.Click)
    def cartAdds[N : Numeric](n: N): EngagementCounts = guard(n)(Engagement.CartAdd)
    def purchases[N : Numeric](n: N): EngagementCounts = guard(n)(Engagement.Purchase)
    private def guard[N](n: N)(e: Engagement)(implicit N: Numeric[N]): EngagementCounts = if(N.gt(n, N.zero)) Map(e -> N.toLong(n)) else Map.empty
  }

  implicit class RichNumeric[N](n: N)(implicit N: Numeric[N]) {
    def binarize: N = if(N.gt(n, N.zero)) N.one else N.zero
  }

  implicit class EngOps(private val engs: EngagementCounts) extends AnyVal {
    def +(e: EngagementCounts): EngagementCounts = engs |+| e
    def countOf(e: Engagement): Long = engs.getOrElse(e, 0)
    def clicks   : Long = countOf(Engagement.Click)
    def cartAdds : Long = countOf(Engagement.CartAdd)
    def purchases: Long = countOf(Engagement.Purchase)
  }

  private[metrics] val log2p1 = (i: Int) => log(i + 1.0) / log(2.0)
  private[metrics] val powOf: Double => Double => Double = e => i => pow(e, i) - 1.0
  private[metrics] val pow2 = powOf(2.0)
  private[metrics] val safeDivide: (Double, Double) => Try[Double] =
    (a, b) => Try(a / b)
      .filter(p =>
        p != Double.NaN ||
          p != Double.PositiveInfinity ||
          p != Double.NegativeInfinity
      )
  private[metrics] val calc = (a: Int, b: Int) => safeDivide(a.toDouble, b.toDouble).toOption

  implicit val nelResultSet: ResultSet[NonEmptyList[Long]] = identity
}
