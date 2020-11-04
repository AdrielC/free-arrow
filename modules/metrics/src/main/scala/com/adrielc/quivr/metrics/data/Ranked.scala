package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import cats.Functor
import com.adrielc.quivr.metrics.ranking.GradedRelevance
import com.adrielc.quivr.metrics.result.AtK
import eu.timepit.refined.cats._

/**
 * He showed that the application of Reciprocal Rank, O-measure (Sakai 2006c) and P+-measure
 * (Sakai 2006a, 2007e) to condensed lists is an effective way of handling the relevance
 * data incompleteness problem.
 */
case class Ranked[+A] private[metrics](indexes: NonEmptyMap[Rank, A], k: Rank) {

  def map[B](f: A => B): Ranked[B] =
    copy(indexes = indexes.map(f))
}

object Ranked {

  def apply[A](results: NonEmptyMap[Rank, A]): Ranked[A] =
    new Ranked(results, results.last._1)

  def apply[A](results: NonEmptyList[A]): Ranked[A] =
    Ranked(results.mapWithIndex((id, idx) => Rank.fromIndex(idx) -> id).toNem)

  def at[A](h: (Rank, A), t: (Rank, A)*): Ranked[A] =
    Ranked(NonEmptyList.of(h, t:_*).toNem)

  def of[A](h: A, t: A*): Ranked[A] =
    Ranked(NonEmptyList.of(h, t:_*))

  implicit val indexesFunctor: Functor[Ranked[*]] = new Functor[Ranked[*]] {
    def map[A, B](fa: Ranked[A])(f: A => B): Ranked[B] =
      fa.map(f)
  }

  implicit def indexesToK[A]: AtK[Ranked[A]] =
    (a, k) => if(k > a.k) None else Some(a.copy(k = k))

  implicit val rankedLabels: GradedRelevance[Ranked[Label]] =
    a => NonEmptyList.fromListUnsafe(a.indexes.toNel.toList.filter(_._1 <= a.k).map(_._2))
}