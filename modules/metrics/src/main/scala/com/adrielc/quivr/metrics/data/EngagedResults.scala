package com.adrielc.quivr.metrics
package data

import cats.data.{EitherNel, NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.metrics.result.AtK
import AtK.ops._
import cats.Order
import cats.implicits._
import com.adrielc.quivr.metrics.result.Results.NonEmptyResults

import scala.collection.immutable.SortedMap

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

  def fromResultsWithEngagements[A : Results : Engagements[*, E], E: Order](engagedResults: A): EitherNel[EmptyError, EngagedResults[E]] = {

    val results = Results[A].results(engagedResults)

    val engagements = Engagements[A, E].engagements(engagedResults)

    val withRes = NonEmptyVector.fromVector(results)

    val withEng = NonEmptyMap.fromMap(SortedMap(engagements.toList.mapFilter(_.traverse(KeyCounts.fromMap[E])):_*))

    (withRes.toRightNel(EmptyError.NoResults),
      withEng.toRightNel(EmptyError.NoEngagements))
      .parMapN(EngagedResults(_, _))
  }

  sealed abstract class EmptyError(override val getMessage: String) extends Error with Product with Serializable
  object EmptyError {
    case object NoResults         extends EmptyError("Results was empty")
    case object NoEngagements     extends EmptyError("No non-zero engagement counts were found")
  }

  implicit def atKEngagedResults[E]: AtK[EngagedResults[E]] = (a, k) => a.results.atK(k).map(n => a.copy(results = n))

  implicit def resultSetEngagedResults[E]: Results[EngagedResults[E]] with Engagements[EngagedResults[E], E] =
    new NonEmptyResults[EngagedResults[E]] with Engagements[EngagedResults[E], E] {
      def engagements(a: EngagedResults[E]): Map[ResultId, Map[E, Int]] = a.engagements.toSortedMap.mapValues(_.toMap)
      def nonEmptyResults(a: EngagedResults[E]): NonEmptyVector[ResultId] = a.results
    }
}