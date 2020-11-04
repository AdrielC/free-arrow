package com.adrielc.quivr.metrics
package data

import cats.{Functor, SemigroupK}
import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._
import com.adrielc.quivr.metrics.ranking.GradedRelevance
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Qrels, ResultLabels, Results}

sealed trait Judged[+A] {
  import Judged.{WithLabels, WithGroundTruth}

  def map[B](f: A => B): Judged[B] = this match {
    case WithGroundTruth(results, relevant) => WithGroundTruth(f(results), relevant)
    case WithLabels(results, labels) => WithLabels(f(results), labels)
  }
}
object Judged {

  case class WithGroundTruth[+A](results: A, groundTruth: NonEmptySet[ResultId]) extends Judged[A]
  case class WithLabels[+A](results: A, labels: NonEmptyMap[ResultId, Label]) extends Judged[A]


  object WithGroundTruth {

    def apply[A: Qrels](results: A): WithGroundTruth[A] =
      WithGroundTruth(results, results.qrels.set)

    def fromLabels[A, V](a: A, labels: Map[ResultId, V], judgeLabel: V => Boolean): Option[WithGroundTruth[A]] =
      labels.toList
        .mapFilter { case (id, v) => judgeLabel(v).guard[Option].as(id) }
        .toNel.map(n => WithGroundTruth(a, n.toNes))

    def fromResultLabels[A: ResultLabels](a: A, judgeLabel: Label => Boolean): Option[WithGroundTruth[A]] =
      WithGroundTruth.fromLabels(a, a.resultLabels.toSortedMap, judgeLabel)

    implicit def withRelGroundTruthSet[A]: Qrels[WithGroundTruth[A]] = a => Qrels.QrelSet(a.groundTruth)
    implicit def resultsWithRelevant[A: AtK]: AtK[WithGroundTruth[A]] = (a, k) => a.results.atK(k).map(WithGroundTruth(_, a.groundTruth))
    implicit def withRelResultSet[A: Results]: Results[WithGroundTruth[A]] = _.results.results
    implicit def withLabelled[A: GradedRelevance]: GradedRelevance[WithGroundTruth[A]] = _.results.relevanceLabels
    implicit def withRelEngagements[A: Engagements[*, E], E]: Engagements[WithGroundTruth[A], E] = _.results.engagementCounts

    implicit val semigroupKWithGroundTruth: SemigroupK[WithGroundTruth] = new SemigroupK[WithGroundTruth] {
      def combineK[A](x: WithGroundTruth[A], y: WithGroundTruth[A]): WithGroundTruth[A] =
        WithGroundTruth(x.results, x.groundTruth union y.groundTruth)
    }
  }

  object WithLabels {

    def apply[A: ResultLabels](results: A): WithLabels[A] =
      WithLabels(results, results.resultLabels)

    def fromGroundTruth[A: Qrels](a: A, groundTruthLabel: Label): WithLabels[A] =
      WithLabels(a, a.qrels.set.map(_ -> groundTruthLabel).toNonEmptyList.toNem)

    def fromLabels[A, V](a: A, resultLabels: Map[ResultId, V], toLabel: V => Option[Label]): Option[WithLabels[A]] =
      resultLabels.toList
        .mapFilter { case (id, e) => toLabel(e).map(id -> _) }.toNel
        .map(n => WithLabels(a, n.toNem))

    implicit def withEngagementsInstance[A]: ResultLabels[WithLabels[A]] = _.labels
    implicit def resultsWithLabels[A: AtK]: AtK[WithLabels[A]] = (a, k) => a.results.atK(k).map(WithLabels(_, a.labels))
    implicit def withLabelsResultSetInstance[A: Results]: Results[WithLabels[A]] = _.results.results
    implicit def withLabGroundTruthSet[A: Qrels]: Qrels[WithLabels[A]] = _.results.qrels
    implicit def withLabEngagements[A: Engagements[*, E], E]: Engagements[WithLabels[A], E] = _.results.engagementCounts

    implicit val semigroupKLabels: SemigroupK[WithLabels] = new SemigroupK[WithLabels] {
      def combineK[A](x: WithLabels[A], y: WithLabels[A]): WithLabels[A] =
        WithLabels(x.results, NonEmptyMap.fromMapUnsafe(x.labels.toSortedMap |+| y.labels.toSortedMap))
    }
  }

  implicit val judgedFunctor: Functor[Judged] = new Functor[Judged] {
    def map[A, B](fa: Judged[A])(f: A => B): Judged[B] = fa.map(f)
  }

  implicit def atKJudged[A: AtK]: AtK[Judged[A]] = (a, k) => a match {
    case g @ WithGroundTruth(_, _)  => g.atK(k)
    case l @ WithLabels(_, _)       => l.atK(k)
  }
}

