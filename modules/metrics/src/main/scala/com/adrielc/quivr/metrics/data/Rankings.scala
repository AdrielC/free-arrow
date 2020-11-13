package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import cats.Functor
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.ranking.{RankedRelevancies, ResultJudgements}
import com.adrielc.quivr.metrics.result.{AtK, BinaryRelevancy, Relevancy}
import com.adrielc.quivr.metrics.retrieval.RelevanceCount
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._

sealed trait Rankings[+A] {
  def ranked: Rankings.Ranked[A]
  def setK(k: Int): Option[Rankings[A]]
  def map[B](f: A => B): Rankings[B]
}
object Rankings {
  import result.{Engagements, Results}
  import cats.implicits._
  import implicits._

  /**
   * He showed that the application of Reciprocal Rank, O-measure (Sakai 2006c) and P+-measure
   * (Sakai 2006a, 2007e) to condensed lists is an effective way of handling the relevance
   * data incompleteness problem.
   */
  final case class Ranked[+A] private[metrics] (rankings: NonEmptyMap[Rank, A], k: Rank) extends Rankings[A] {

    override def ranked: Ranked[A] = this

    override def setK(newK: Int): Option[Rankings[A]] =
      if(newK > k) None else Rank.from(newK).toOption.map(newK => copy(k = newK))

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

    def toRankMap[A](results: NonEmptyVector[(_, A)]): NonEmptyMap[Rank, A] =
      results.mapWithIndex((id, idx) => Rank.fromIndex(idx) -> id._2).toNem

    implicit def indexesToK[A]: AtK[Ranked[A]] =
      (a, k) => if(k > a.k) None else Rank.from(k).toOption.map(newK => a.copy(k = newK))

    implicit def rankedLabels[R: Relevancy]: RankedRelevancies.Aux[Ranked[R], R] =
      new RankedRelevancies[Ranked[R]] {
        override type Rel = R
        val relevancy: Relevancy[R] = Relevancy[R]
        override def rankedRelevancies(a: Ranked[R]): Ranked[R] = a
      }
  }

  final case class RankedResults[+A] private (res: NonEmptyVector[(ResultId, A)], k: Rank, nRelevant: Count) extends Rankings[A] {

    override lazy val ranked: Ranked[A] =
      Ranked(Ranked.toRankMap(res), k)

    override def map[B](f: A => B): RankedResults[B] =
      RankedResults(res.map { case (id, a) => id -> f(a) }, k, nRelevant)

    lazy val resultsAtK: NonEmptyVector[(ResultId, A)] =
      NonEmptyVector.fromVectorUnsafe(res.toVector.take(k.value)) // safe because k is guaranteed >= 1

    override def setK(k: Int): Option[Rankings[A]] =
      Rank.from(k).toOption.flatMap(newK => (newK <= res.length).guard[Option].as(copy(k = newK)))

    def setK(newK: Rank): Option[RankedResults[A]] =
      (newK <= res.length).guard[Option].as(RankedResults(res, newK, nRelevant))
  }

  object RankedResults {

    // guarantees that if a list of relevancies is returned then there is at least one judged result and one relevant
    def apply[A: Engagements[*, E]: Results, E](a: A, judgeLabel: Map[E, Int] => Relevance): Option[RankedResults[Relevance]] = {
      val rels = a.engagementCounts.mapValues(judgeLabel)
      val results = a.results.map(id => (id, rels.getOrElse(id, Relevance.unjudged)))
      (Some(results.toList.count(_._2.gain.isDefined)).filter(_ > 0) *>
        Count.from(rels.count(a => a._2.gain.gain > 0)).toOption.filter(_ > 0))
        .map(new RankedResults(results.map(a => a._1 -> a._2), Rank.unsafeFrom(results.length), _))
    }

    implicit def resultRelsRelevanciesInstance[A: BinaryRelevancy]: ResultJudgements.Aux[RankedResults[A], (ResultId, A)] with RelevanceCount[RankedResults[A]] =
      new ResultJudgements[RankedResults[A]] with RelevanceCount[RankedResults[A]] {

        override type Rel = (ResultId, A)

        override val relevancy: BinaryRelevancy[(ResultId, A)] = BinaryRelevancy[A].contramap(_._2)

        override def resultRelevancies(a: RankedResults[A]): NonEmptyVector[(ResultId, A)] = a.resultsAtK

        override def groundTruthCount(a: RankedResults[A]): Int = a.nRelevant

        override def truePositiveCount(a: RankedResults[A]): Int =
          a.res.toVector.foldLeft((1, 0)) { case ((rnk, cnt), (id, rel)) =>
            (rnk + 1) -> (if(rnk <= a.k && relevancy.isRel((id, rel))) cnt + 1 else cnt )
          }._2
      }

    implicit def rankedResultsAtK[A]: AtK[RankedResults[A]] = (a, k) => Rank.from(k).toOption.flatMap(a.setK)
  }
}