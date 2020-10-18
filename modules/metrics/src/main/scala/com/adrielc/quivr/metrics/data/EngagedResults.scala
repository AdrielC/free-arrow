package com.adrielc.quivr.metrics
package data

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}


case class EngagedResults[E](results: NonEmptyList[ResultId], engagements: Map[ResultId, KeyCounts[E]]) {

  def binarize: EngagedResults[E] =
    copy(engagements = engagements.mapValues(_.binarize))
}
object EngagedResults {

  implicit def atKEngagedResults[E]: AtK[EngagedResults[E]] = (a, k) => a.results.atK(k).map(n => a.copy(results = n))

  implicit def resultSetEngagedResults[E]: Results[EngagedResults[E]] with Engagements[EngagedResults[E], E] =
    new Results[EngagedResults[E]] with Engagements[EngagedResults[E], E] {
      def engagementCounts(a: EngagedResults[E]): Map[ResultId, KeyCounts[E]] = a.engagements
      def results(a: EngagedResults[E]): NonEmptyList[ResultId] = a.results
    }
}