package com.adrielc.quivr.metrics

import cats.Functor
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import com.adrielc.quivr.metrics.LabelOp.free.MissingLabels

object evaluable {

  type ResultsWithEngagements = Results[EngagementCounts]

  type ResultsWithLabels = Results[Label]

  type ResultsWithRelevant = WithRelevant[NonEmptyList[ResultId]]

  case class LabelledIndexes private (indexedLabels: NonEmptyMap[Index, Label], k: Int) {

    val maxK: Index = indexedLabels.last._1

    def idealRanking: LabelledIndexes =
      new LabelledIndexes(
        indexedLabels.toNel.sortBy(-_._2).mapWithIndex { case ((_, l), i) => (i + 1) -> l }.toNem,
        k
      )
  }
  object LabelledIndexes {

    def apply(indexedLabels: NonEmptyList[(Index, Label)]): LabelledIndexes =
      new LabelledIndexes(indexedLabels.toNem, indexedLabels.toList.maxBy(_._1)._1)

    def apply(r: ResultsWithLabels): Either[MissingLabels, LabelledIndexes] =
      r.results
        .zipWithIndex.toList
        .mapFilter { case ((_, label), i) => label.map((i + 1) -> _) }
        .toNel.map(LabelledIndexes(_))
        .toRight(MissingLabels)

    def of(h: (Index, Label), t: (Index, Label)*): LabelledIndexes =
      LabelledIndexes(NonEmptyList.of(h, t:_*).sortBy(_._1))

    def labels(h: Label, t: Label*): LabelledIndexes =
      of(1 -> h, t.toList.mapWithIndex((l, i) => (i + 1) -> l):_*)


    implicit val toKLabelledResult: ToK[LabelledIndexes] = new ToK[LabelledIndexes] {
      def toK(a: LabelledIndexes, k: Index): Option[LabelledIndexes] =
        if(k > a.maxK) None else Some(a.copy(k = k))

      def maxK(a: LabelledIndexes): Index =
        a.maxK
    }
  }

  case class Results[+A](results: NonEmptyList[(ResultId, Option[A])])
  object Results {

    def of[A](h: (ResultId, Option[A]), t: (ResultId, Option[A])*): Results[A] =
      new Results(NonEmptyList.of(h, t:_*))

    implicit val resultsFunctor: Functor[Results] = new Functor[Results] {
      def map[A, B](fa: Results[A])(f: A => B): Results[B] =
        Results(fa.results.map { case (r, l) => r -> l.map(f) })
    }

    implicit def resultsToK[A]: ToK[Results[A]] = new ToK[Results[A]] {
      def toK(a: Results[A], k: Index): Option[Results[A]] =
        a.results.toList.take(k).toNel.map(Results(_))

      def maxK(a: Results[A]): Index =
        a.results.size
    }
  }

  case class WithRelevant[A](results: A, relevant: NonEmptySet[ResultId]) {

    def nRelevantResults(implicit ev: A <:< NonEmptyList[ResultId]): Int = results.toNes.intersect(relevant).size
  }
  object WithRelevant {
    implicit def toKRankingWithRelevants[A: ToK]: ToK[WithRelevant[A]] = new ToK[WithRelevant[A]] {
      def toK(a: WithRelevant[A], k: Index): Option[WithRelevant[A]] =
        a.results.toK(k).map(WithRelevant(_, a.relevant))

      def maxK(a: WithRelevant[A]): Index =
        a.results.maxK
    }
  }
}
