package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptySet}

case class WithRelevant[+A](results: A, relevant: NonEmptySet[ResultId])

object WithRelevant {

  implicit def toKRankingWithRelevants[A: ToK]: ToK[WithRelevant[A]] =
    (a, k) => a.results.toK(k).map(WithRelevant(_, a.relevant))

  implicit def withRelevantRelevantCounts[A: ResultSet]: RelevantResultSet[WithRelevant[A]] = new RelevantResultSet[WithRelevant[A]] {

    def relevant(a: WithRelevant[A]): NonEmptySet[Count] = a.relevant

    def resultIds(a: WithRelevant[A]): NonEmptyList[Count] = a.results.resultIds
  }

  implicit val resultsWithRelevantInstances: RelevantResultSet[ResultsWithRelevant] = new RelevantResultSet[ResultsWithRelevant] {

    def relevant(a: ResultsWithRelevant): NonEmptySet[ResultId] = a.relevant

    def resultIds(a: ResultsWithRelevant): NonEmptyList[ResultId] = a.results.resultIds
  }

  implicit val resultsWithRelevant: ToK[ResultsWithRelevant] =
    (a, k) => if(k > a.results.k) None else Some(a.copy(results = a.results.copy(k = k)))
}
