package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.metrics.result.AtK
import AtK.ops._
import com.adrielc.quivr.metrics.dsl.EngRes

/**
 * Witnesses the following
 * 1: At least one result
 * 2: At least one result with an engagement of type [[E]]
 */
case class EngagedResults[E](
  results     : NonEmptyVector[ResultId],
  engagements : NonEmptyMap[ResultId, E]
)
object EngagedResults {
  import result.{Results, Engagements}

  implicit def atKEngagedResults[E]: AtK[EngagedResults[E]] = (a, k) => a.results.atK(k).map(n => a.copy(results = n))

  implicit def resultSetEngagedResults[E]: Results[EngRes[E]] with Engagements[EngRes[E], E] =
    new Results[EngRes[E]] with Engagements[EngRes[E], E] {
      def engagements(a: EngRes[E]): Map[ResultId, Map[E, Int]] = a.engagements.toSortedMap.mapValues(_.toMap)
      def results(a: EngRes[E]): NonEmptyVector[ResultId] = a.results
    }
}