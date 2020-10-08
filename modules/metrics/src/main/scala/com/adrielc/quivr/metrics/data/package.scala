package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

import scala.collection.immutable.SortedSet

package object data {

  type LabelledResults = IndexedResults[Double]

  type ResultsWithRelevant = WithRelevant[IndexedResults[Boolean]]
  object ResultsWithRelevant {

    def apply(results: Results, relevant: NonEmptySet[ResultId]): ResultsWithRelevant =
      WithRelevant(IndexedResults((results: NonEmptyMap[Index, ResultId]).map(id => id -> relevant.contains(id))), relevant)

    def apply(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId]): ResultsWithRelevant =
      ResultsWithRelevant(results.mapWithIndex((id, idx) => idx -> id).toNem, relevant)

    def apply(results: NonEmptyList[ResultId], relevant: TraversableOnce[ResultId]): Option[ResultsWithRelevant] =
      NonEmptySet.fromSet(SortedSet(relevant.toList:_*)).map(ResultsWithRelevant(results, _))

    def apply(r: ResultId, rs: ResultId*)(rl: ResultId, rls: ResultId*): ResultsWithRelevant =
      ResultsWithRelevant(NonEmptyList(r, rs.toList), NonEmptySet.fromSetUnsafe(SortedSet(rl +: rls:_*)))
  }

  type EngagedResults = IndexedResults[Option[EngagementCounts]]
  object EngagedResults {
    def apply(results: NonEmptyList[ResultId], engagements: Map[ResultId, EngagementCounts]): EngagedResults = {
      val resSet = results.toNes
      val k = results.size

      lazy val labelsNotInResultSet = engagements.toList
        .mapFilter { case (id, e) => (!resSet.contains(id)).guard[Option].as(id -> Option(e)) }
        .mapWithIndex((label, idx) => (Int.MaxValue - idx) -> label )

      val nem = results.mapWithIndex((id, idx) => (idx + 1, (id, engagements.get(id)))).toNem
      val added = labelsNotInResultSet.foldLeft(nem)(_ add _)
      IndexedResults(added, k)
    }
  }


  private[metrics] def nonInf(d: Double): Option[Double] = Option(d).filterNot(_.abs == Double.PositiveInfinity)
}
