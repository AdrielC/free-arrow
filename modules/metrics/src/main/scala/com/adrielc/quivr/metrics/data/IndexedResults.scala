package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import cats.{Functor, Id}

case class IndexedResults[+A] private(results: NonEmptyMap[Index, (ResultId, A)], k: Int)
object IndexedResults {

  def apply[A](results: NonEmptyMap[Index, (ResultId, A)]): IndexedResults[A] =
    new IndexedResults(results, results.last._1)

  def of[A](h: (ResultId, A), t: (ResultId, A)*): IndexedResults[A] =
    IndexedResults(NonEmptyList.of(h, t:_*).mapWithIndex { case (v, idx) => (idx + 1) -> (v: (ResultId, Id[A])) }.toNem)

  implicit def resultsFunctor: Functor[IndexedResults[+*]] = new Functor[IndexedResults[+*]] {
    def map[A, B](fa: IndexedResults[A])(f: A => B): IndexedResults[B] =
      IndexedResults(fa.results.map { case (l, r) => l -> f(r) })
  }
  implicit def resultsToK[A]: ToK[IndexedResults[A]] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))
}