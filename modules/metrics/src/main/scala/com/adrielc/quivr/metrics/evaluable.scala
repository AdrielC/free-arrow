package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

object evaluable {

  case class LabelledIndexes private (indexedLabels: NonEmptyList[(Index, Label)], k: Int) {

    def toK(newK: Int): Option[LabelledIndexes] = indexedLabels.filter(_._1 <= newK).toNel.map(LabelledIndexes(_, newK))
  }
  object LabelledIndexes {

    def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes = new LabelledIndexes(indexedLabels, indexedLabels.toList.maxBy(_._1)._1)

    def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes = LabelledIndexes(NonEmptyList.of(h, t:_*).sortBy(_._1))

    def labels(h: Label, t: Label*): LabelledIndexes = of(1 -> h, t.toList.mapWithIndex((l, i) => (i + 1) -> l):_*)

    implicit val toKLabelledResult: ToK[LabelledIndexes] = new ToK[LabelledIndexes] {
      def toK(a: LabelledIndexes, k: Index): Option[LabelledIndexes] = a.toK(k)
      def maxK(a: LabelledIndexes): Index = a.k
    }
  }

  case class ResultsWithLabels(results: NonEmptyList[ResultId], labels: NonEmptyMap[ResultId, Label])
  object ResultsWithLabels {

    implicit val toKResultsWithLabels: ToK[ResultsWithLabels] = new ToK[ResultsWithLabels] {
      def toK(a: ResultsWithLabels, k: Index): Option[ResultsWithLabels] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithLabels): Index = a.results.size
    }
  }

  case class ResultsWithRelevant(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId]) {
    lazy val nRelevantResults: Int = results.toNes.intersect(relevant).size
  }
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
