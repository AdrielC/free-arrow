package com.adrielc.quivr.metrics
package data

import cats.Functor
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Judged.{LabelsAndRelevant, WithLabels, WithGroundTruth}

sealed trait Judged[+A] {

  def map[B](f: A => B): Judged[B] = this match {
    case Judged.WithGroundTruth(results, relevant) => WithGroundTruth(f(results), relevant)
    case Judged.WithLabels(results, labels) => WithLabels(f(results), labels)
    case Judged.LabelsAndRelevant(results, labels, relevant) => LabelsAndRelevant(f(results), labels, relevant)
  }
}
object Judged {

  case class WithGroundTruth[+A](results: A, groundTruth: NonEmptySet[ResultId]) extends Judged[A]
  object WithGroundTruth {

    implicit def resultsWithRelevant[A: AtK]: AtK[WithGroundTruth[A]] =
      (a, k) => a.results.atK(k).map(WithGroundTruth(_, a.groundTruth))

    implicit def withRelGroundTruthSet[A]: GroundTruthSet[WithGroundTruth[A]] = _.groundTruth

    implicit def withLabelled[A: LabelledSet]: LabelledSet[WithGroundTruth[A]] = _.results.labelledSet

    implicit def withRelResultSet[A: ResultSet]: ResultSet[WithGroundTruth[A]] = _.results.results
  }

  case class WithLabels[+A](results: A, labels: NonEmptyMap[ResultId, Label]) extends Judged[A]
  object WithLabels {

    implicit def withEngagementsInstance[A]: ResultLabels[WithLabels[A]] = _.labels

    implicit def withLabelsResultSetInstance[A: ResultSet]: ResultSet[WithLabels[A]] = _.results.results

    implicit def withLabGroundTruthSet[A: GroundTruthSet]: GroundTruthSet[WithLabels[A]] = _.results.groundTruth

    implicit def resultsWithLabels[A: AtK]: AtK[WithLabels[A]] =
      (a, k) => a.results.atK(k).map(WithLabels(_, a.labels))
  }

  case class LabelsAndRelevant[+A](results: A, labels: NonEmptyMap[ResultId, Label], relevant: NonEmptySet[ResultId]) extends Judged[A]
  object LabelsAndRelevant {

    implicit def labelledAndRelevant[A: ResultSet]: LabelledSet[LabelsAndRelevant[A]] with RelevanceJudgements[LabelsAndRelevant[A]] =
      new LabelledSet[LabelsAndRelevant[A]] with RelevanceJudgements[LabelsAndRelevant[A]] {
        def labelledSet(a: LabelsAndRelevant[A]): NonEmptyList[Label] = a.results.results.map(a.labels.lookup(_).getOrElse(0.0))
        def relevanceJudgements(a: LabelsAndRelevant[A]): NonEmptyList[Boolean] = a.results.results.map(a.relevant.contains)
        override def resultCount(a: LabelsAndRelevant[A]): Int = ResultSet[A].resultCount(a.results)
      }

  }

  implicit def withRelevantFunctor[F[_]: Functor]: Functor[λ[α => Judged[F[α]]]] = new Functor[λ[α => Judged[F[α]]]] {
    def map[A, B](fa: Judged[F[A]])(f: A => B): Judged[F[B]] = fa.map(_.map(f))
  }
}

