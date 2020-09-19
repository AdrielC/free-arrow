package com.adrielc.arrow.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

object evaluable {

  case class LabelledIndexes private (indexedLabels: NonEmptyList[(Index, Label)], k: Int) {
    val indexes: NonEmptySet[Index] = indexedLabels.map(_._1).toNes
    val maxK: Index = indexes.maximum

    def setK(_k: Int): LabelledIndexes = {
      assert(_k > 0)
      copy(k = _k)
    }

    def filterK: Option[LabelledIndexes] = indexedLabels.filter(_._1 <= k).toNel.map(LabelledIndexes(_, k))
  }
  object LabelledIndexes {

    def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes = new LabelledIndexes(indexedLabels, indexedLabels.toList.maxBy(_._1)._1)

    def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes = LabelledIndexes(NonEmptyList.of(h, t:_*).sortBy(_._1), h._1 max t.maxBy(_._1)._1)

    def labels(h: Label, t: Label*): LabelledIndexes = LabelledIndexes(NonEmptyList.of(h, t:_*).mapWithIndex((l, i) => (i + 1) -> l))

    implicit val toKLabelledResult: ToK[LabelledIndexes] = new ToK[LabelledIndexes] {
      def toK(a: LabelledIndexes, k: Index): Option[LabelledIndexes] = Some(a.setK(k))
      def maxK(a: LabelledIndexes): Index = a.maxK
    }
  }

  case class ResultsWithLabels(results: NonEmptyList[ResultId], labels: NonEmptyMap[ResultId, Label])
  object ResultsWithLabels {

    implicit val toKResultsWithLabels: ToK[ResultsWithLabels] = new ToK[ResultsWithLabels] {
      def toK(a: ResultsWithLabels, k: Index): Option[ResultsWithLabels] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithLabels): Index = a.results.size
    }
  }

  case class ResultsWithRelevant(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId])
  object ResultsWithRelevant {

    implicit val toKRankingWithRelevants: ToK[ResultsWithRelevant] = new ToK[ResultsWithRelevant] {
      def toK(a: ResultsWithRelevant, k: Index): Option[ResultsWithRelevant] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithRelevant): Index = a.results.size
    }
  }

  case class ResultsWithEngagements(results: NonEmptyList[ResultId], engagements: Map[ResultId, EngagementCounts])
  object ResultsWithEngagements {

    implicit val toKResultsWithEngagements: ToK[ResultsWithEngagements] = new ToK[ResultsWithEngagements] {
      def toK(a: ResultsWithEngagements, k: Index): Option[ResultsWithEngagements] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithEngagements): Index = a.results.size
    }
  }
}
