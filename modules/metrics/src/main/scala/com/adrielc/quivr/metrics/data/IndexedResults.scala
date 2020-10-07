package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import cats.{Functor, Id}

case class IndexedResults[+A] private (results: NonEmptyMap[Index, (ResultId, A)], k: Int) {

  def map[B](f: A => B): IndexedResults[B] =
    copy(results = results.map { case (id, a) => (id, f(a)) })

  def mapWithId[B](f: (ResultId, A) => B): IndexedResults[B] =
    copy(results = results.map { case (id, a) => id -> f(id, a) })
}
object IndexedResults {

  def apply[A](results: NonEmptyMap[Index, (ResultId, A)]): IndexedResults[A] =
    new IndexedResults(results, results.last._1)

  def apply[A](results: NonEmptyList[ResultId]): IndexedResults[Unit] =
    new IndexedResults(results.mapWithIndex((id, idx) => ((idx + 1), (id, ()))).toNem, results.length)

  def of[A](h: (ResultId, A), t: (ResultId, A)*): IndexedResults[A] =
    IndexedResults(NonEmptyList.of(h, t:_*).mapWithIndex { case (v, idx) => (idx + 1) -> (v: (ResultId, Id[A])) }.toNem)

  implicit val resultsFunctor: Functor[IndexedResults[+*]] = new Functor[IndexedResults[+*]] {
    def map[A, B](fa: IndexedResults[A])(f: A => B): IndexedResults[B] =
      fa.map(f)
  }

  implicit def resultsToK[A]: ToK[IndexedResults[A]] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))


  implicit def resultSet[A]: ResultSet[IndexedResults[A]] = _.results.toNel.map(_._2._1)

  implicit val labelledIndexesInstances: IndexedLabels[LabelledResults] = a => LabelledIndexes(a.results.map(_._2), a.k)

  implicit val toKLabelledResult: ToK[LabelledResults] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))

  implicit class LabelledIndexesOps(private val l: LabelledResults) extends AnyVal {

    def ideal: LabelledResults =
      l.copy(results = l.results.toNel.sortBy(-_._2._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }.toNem)

    def dcg: Double = l.results.toNel.foldMap { case (i, (_, rel)) => if(i <= l.k) rel / log2p1(i) else 0.0 }

    def ndcg: Option[Double] = safeDivide(dcg, ideal.dcg).toOption
  }
}