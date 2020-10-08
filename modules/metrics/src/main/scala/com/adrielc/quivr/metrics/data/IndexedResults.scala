package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import cats.Functor

case class IndexedResults[+A] private (results: NonEmptyMap[Index, (ResultId, A)], k: Int) {

  def map[B](f: A => B): IndexedResults[B] =
    copy(results = results.map { case (id, a) => (id, f(a)) })

  def mapWithId[B](f: (ResultId, A) => B): IndexedResults[B] =
    copy(results = results.map { case (id, a) => id -> f(id, a) })
}
object IndexedResults {

  def apply[A](results: NonEmptyMap[Index, (ResultId, A)]): IndexedResults[A] =
    new IndexedResults(results, results.last._1)

  def apply[A](results: NonEmptyList[(ResultId, A)]): IndexedResults[A] =
    IndexedResults(results.mapWithIndex((id, idx) => (idx + 1, id)).toNem)

  def of[A](h: (ResultId, A), t: (ResultId, A)*): IndexedResults[A] =
    IndexedResults(NonEmptyList.of(h, t:_*))

  implicit val resultsFunctor: Functor[IndexedResults[+*]] = new Functor[IndexedResults[+*]] {
    def map[A, B](fa: IndexedResults[A])(f: A => B): IndexedResults[B] =
      fa.map(f)
  }

  implicit def resultsToK[A]: ToK[IndexedResults[A]] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))

  implicit def resultSet[A]: ResultSet[IndexedResults[A]] =
    _.results.toNel.map(_._2._1)

  implicit val labelledIndexesInstances: IndexedLabels[LabelledResults] =
    a => LabelledIndexes(a.results.map(_._2), a.k)
}