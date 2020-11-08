package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.metrics.result.AtK, AtK.ops._

/**
 * Witnesses the following
 * 1: At least one result
 * 2: At least one result with an engagement of type [[E]]
 * 3: The one required engagement must have a count of 1 or greater
 */
case class EngagedResults[E](results: NonEmptyVector[ResultId], engagements: NonEmptyMap[ResultId, KeyCounts[E]]) {

  def binarize: EngagedResults[E] =
    copy(engagements = engagements.map(_.binarize))
}
object EngagedResults {
  import result.{Results, Engagements}

  implicit def atKEngagedResults[E]: AtK[EngagedResults[E]] = (a, k) => a.results.atK(k).map(n => a.copy(results = n))

  implicit def resultSetEngagedResults[E]: Results[EngagedResults[E]] with Engagements[EngagedResults[E], E] =
    new Results[EngagedResults[E]] with Engagements[EngagedResults[E], E] {
      def engagements(a: EngagedResults[E]): Map[ResultId, Map[E, Int]] = a.engagements.toSortedMap.mapValues(_.toMap)
      def results(a: EngagedResults[E]): NonEmptyVector[ResultId] = a.results
    }
}