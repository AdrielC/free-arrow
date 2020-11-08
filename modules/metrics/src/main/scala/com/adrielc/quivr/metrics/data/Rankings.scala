package com.adrielc.quivr
package metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import cats.implicits._
import cats.{Eq, Functor, Monoid}
import com.adrielc.quivr.metrics.ranking.{RankedRelevancies, ResultRelevancies}
import com.adrielc.quivr.metrics.result.{AtK, Relevancy}
import com.adrielc.quivr.metrics.retrieval.RelevanceCount
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._

sealed trait Rankings[+A] {
  def rankings: NonEmptyMap[Rank, A]
  def map[B](f: A => B): Rankings[B]
}
object Rankings {

  /**
   * He showed that the application of Reciprocal Rank, O-measure (Sakai 2006c) and P+-measure
   * (Sakai 2006a, 2007e) to condensed lists is an effective way of handling the relevance
   * data incompleteness problem.
   */
  final case class Ranked[+A] private[metrics] (rankings: NonEmptyMap[Rank, A], k: Rank) extends Rankings[A] {

    def map[B](f: A => B): Ranked[B] =
      copy(rankings = rankings.map(f))
  }

  object Ranked {

    def apply[A](results: NonEmptyMap[Rank, A]): Ranked[A] =
      new Ranked(results, results.last._1)

    def apply[A](results: NonEmptyList[A]): Ranked[A] =
      Ranked(results.mapWithIndex((id, idx) => Rank.fromIndex(idx) -> id).toNem)

    def apply[A](results: NonEmptyVector[A]): Ranked[A] =
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
      (a, k) => if(k > a.k) None else Rank.from(k).toOption.map(newK => a.copy(k = newK))

    implicit def rankedLabels[R: Relevancy]: RankedRelevancies.Aux[Ranked[R], R] =
      new RankedRelevancies[Ranked[R]] {
        override type Rel = R
        val rel: Relevancy[R] = Relevancy[R]
        override def rankedRelevancies(a: Ranked[R]): Ranked[R] = a
      }
  }

  final case class RankedResults[+A] private[metrics] (res: NonEmptyVector[(ResultId, A)], k: Rank, nRelevant: Count) extends Rankings[A] {

    override def rankings: NonEmptyMap[Rank, A] =
      res.zipWithIndex.map { case ((_, a), i) => Rank.fromIndex(i) -> a }.toNem

    override def map[B](f: A => B): RankedResults[B] =
      copy(res = res.map { case (id, a) => id -> f(a) })

    def setK(newK: Rank): Option[RankedResults[A]] =
      (newK <= res.length).guard[Option].as(copy(k = newK))
  }

  object RankedResults {

    import result.{Engagements, Results}
    import cats.implicits._
    import implicits._

    // guarantees that if a list of relevancies is returned then there is at least one judged result
    def apply[A: Engagements[*, E]: Results, E, B: Relevancy : Monoid : Eq](a: A, judgeLabel: Map[E, Int] => B): Option[RankedResults[B]] = {

      val rels = a.engagementCounts.mapValues(judgeLabel)

      val results = a.results.map(id => (id, rels.getOrElse(id, Monoid.empty[B])))

      val nRel = Count.from(rels.count(_._2.isRel)).toOption.filter(_ > 0)

      val nJudged = results.toList.exists(!_._2.isEmpty).guard[Option]

      (nJudged *> nRel).map(new RankedResults(results, Rank.unsafeFrom(results.length), _))
    }

    implicit def resultRelsRelevanciesInstance[A: Relevancy]: ResultRelevancies.Aux[RankedResults[A], (ResultId, A)] with RelevanceCount[RankedResults[A]] =
      new ResultRelevancies[RankedResults[A]] with RelevanceCount[RankedResults[A]] {

        override type Rel = (ResultId, A)

        override val rel: Relevancy[(ResultId, A)] =
          Relevancy.gainRelevancy.contramap(_._2.gainValue)

        override def resultRelevancies(a: RankedResults[A]): NonEmptyVector[(ResultId, A)] =
          NonEmptyVector.fromVectorUnsafe(a.res.toVector.take(a.k.value)) // safe because k is guaranteed >= 1

        override def groundTruthCount(a: RankedResults[A]): Int =
          a.nRelevant
      }

    implicit def rankedResultsAtK[A]: AtK[RankedResults[A]] = (a, k) => Rank.from(k).toOption.flatMap(a.setK)
  }
}