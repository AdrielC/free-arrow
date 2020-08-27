package com.adrielc.arrow.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._

object evaluable {

  case class LabelledIndexes private (indexedLabels: NonEmptyList[(Index, Label)]) {
    lazy val indexes: NonEmptySet[Index] = indexedLabels.map(_._1).toNes
    lazy val maxK: Index = indexes.maximum
  }
  object LabelledIndexes {

    def apply(h: (Index, Label), t: (Index, Label)*): LabelledIndexes = new LabelledIndexes(NonEmptyList.of(h, t:_*))

    implicit val toKLabelledResult: ToK[LabelledIndexes] = new ToK[LabelledIndexes] {
      def toK(a: LabelledIndexes, k: Index): Option[LabelledIndexes] = a.indexedLabels.filter(_._1 <= k).toNel.map(LabelledIndexes(_))
      def maxK(a: LabelledIndexes): Index = a.maxK
    }
  }

  sealed trait Results {
    def results: NonEmptyList[ResultId]
    lazy val size: Index = results.size
  }

  case class ResultsWithLabels(results: NonEmptyList[ResultId], labels: NonEmptyMap[ResultId, Label]) extends Results
  object ResultsWithLabels {

    implicit val toKResultsWithLabels: ToK[ResultsWithLabels] = new ToK[ResultsWithLabels] {
      def toK(a: ResultsWithLabels, k: Index): Option[ResultsWithLabels] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithLabels): Index = a.size
    }
  }

  case class ResultsWithRelevant(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId]) extends Results
  object ResultsWithRelevant {

    implicit val toKRankingWithRelevants: ToK[ResultsWithRelevant] = new ToK[ResultsWithRelevant] {
      def toK(a: ResultsWithRelevant, k: Index): Option[ResultsWithRelevant] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithRelevant): Index = a.size
    }
  }

  case class ResultsWithEngagements(results: NonEmptyList[ResultId], engagements: EngagedResults) extends Results
  object ResultsWithEngagements {

    implicit val toKResultsWithEngagements: ToK[ResultsWithEngagements] = new ToK[ResultsWithEngagements] {
      def toK(a: ResultsWithEngagements, k: Index): Option[ResultsWithEngagements] = a.results.toList.take(k).toNel.map(atK => a.copy(results = atK))
      def maxK(a: ResultsWithEngagements): Index = a.size
    }
  }
}
