package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._

import scala.collection.immutable.SortedSet

case class ResultsWithRelevant(results: NonEmptyList[ResultId], relevant: NonEmptySet[ResultId])
object ResultsWithRelevant {

  def apply(results: NonEmptyList[ResultId], relevant: TraversableOnce[ResultId]): Option[ResultsWithRelevant] =
    NonEmptySet.fromSet(SortedSet(relevant.toList:_*))
      .map(new ResultsWithRelevant(results, _))

  def apply(r: ResultId, rs: ResultId*)(rl: ResultId, rls: ResultId*): ResultsWithRelevant =
    new ResultsWithRelevant(NonEmptyList(r, rs.toList), NonEmptySet.fromSetUnsafe(SortedSet(rl +: rls:_*)))

  implicit val resultsWithRelevantInstances: RelevantResultSet[ResultsWithRelevant] = new RelevantResultSet[ResultsWithRelevant] {

    def relevant(a: ResultsWithRelevant): NonEmptySet[ResultId] = a.relevant

    def resultIds(a: ResultsWithRelevant): NonEmptyList[ResultId] = a.results
  }

  implicit val resultsWithRelevant: ToK[ResultsWithRelevant] =
    (a, k) => a.results.toList.take(k).toNel.map(ResultsWithRelevant(_, a.relevant))
}