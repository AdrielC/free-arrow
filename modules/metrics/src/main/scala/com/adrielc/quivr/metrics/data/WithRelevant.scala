package com.adrielc.quivr.metrics
package data

import cats.data.NonEmptySet
import cats.implicits._

case class WithRelevant[+A](results: A, relevant: NonEmptySet[ResultId]) {

  def nRelevantResults(implicit R: ResultSet[A]): Int = results.resultIds.toNes.intersect(relevant).size

  def precision(implicit R: ResultSet[A]): Double = nRelevantResults / results.nResults.toDouble

  def recall(implicit R: ResultSet[A]): Double = nRelevantResults / relevant.size.toDouble
}
object WithRelevant {

  implicit def toKRankingWithRelevants[A: ToK]: ToK[WithRelevant[A]] =
    (a, k) => a.results.toK(k).map(WithRelevant(_, a.relevant))
}
