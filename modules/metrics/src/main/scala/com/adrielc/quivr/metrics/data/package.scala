package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._

import scala.collection.immutable.SortedSet

package object data {

  type LabelledResults = IndexedResults[Double]
  object LabelledResults {

    def apply(k: Int)(indexedLabels: NonEmptyList[(Index, (ResultId, Label))]): LabelledResults =
      IndexedResults(indexedLabels.toNem, k)

    def apply(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId]): LabelledResults =
      IndexedResults(results.mapWithIndex((a, i) => (i + 1, (a, if(relevant.contains(a)) 1.0 else 0.0 ))).toNem, results.size)

    def apply(r: ResultsWithRelevant): LabelledResults =
      r.results.map(if(_) 1.0 else 0.0)

    def of(h: (Index, (ResultId, Label)), t: (Index, (ResultId, Label))*): LabelledResults =
      IndexedResults(NonEmptyList(h, t.toList).toNem)

    def of(k: Int)(h: (Index, (ResultId, Label)), t: (Index, (ResultId, Label))*): LabelledResults =
      IndexedResults(NonEmptyList.of(h, t:_*).sortBy(_._1).toNem, k)

    def labels(h: (ResultId, Label), t: (ResultId, Label)*): LabelledResults =
      IndexedResults(NonEmptyList(h, t.toList).mapWithIndex((l, idx) => (idx + 1) -> l).toNem)
  }

  type ResultsWithRelevant = WithRelevant[IndexedResults[Boolean]]
  object ResultsWithRelevant {

    def apply(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId]): ResultsWithRelevant =
      WithRelevant(IndexedResults(results).mapWithId((id, _) => relevant.contains(id)), relevant)

    def apply(results: NonEmptyList[ResultId], relevant: TraversableOnce[ResultId]): Option[ResultsWithRelevant] =
      NonEmptySet.fromSet(SortedSet(relevant.toList:_*)).map(ResultsWithRelevant(results, _))

    def apply(r: ResultId, rs: ResultId*)(rl: ResultId, rls: ResultId*): ResultsWithRelevant =
      ResultsWithRelevant(NonEmptyList(r, rs.toList), NonEmptySet.fromSetUnsafe(SortedSet(rl +: rls:_*)))
  }

  type EngagedResults = IndexedResults[Option[EngagementCounts]]
  object EngagedResults {
    def apply(results: NonEmptyList[ResultId], engagements: Map[ResultId, EngagementCounts]): EngagedResults = {
      val resSet = results.toNes

      lazy val labelsNotInResultSet = engagements.toList
        .mapFilter { case (id, e) => (!resSet.contains(id)).guard[Option].as(id -> Option(e)) }
        .mapWithIndex((label, idx) => (Int.MaxValue - idx) -> label )

      val nem = results.mapWithIndex((id, idx) => (idx + 1, (id, engagements.get(id)))).toNem
      val added = labelsNotInResultSet.foldLeft(nem)(_ add _)
      IndexedResults(added, results.size)
    }
  }
}
