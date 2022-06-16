package com.adrielc.quivr.metrics.data

import cats.{Functor, SemigroupK}
import cats.data.{NonEmptyMap, NonEmptySet, NonEmptyVector}
import com.adrielc.quivr.metrics.ResultId
import com.adrielc.quivr.metrics.ranking.ResultRelevancies
import com.adrielc.quivr.metrics.result.Results.NonEmptyResults
import com.adrielc.quivr.metrics.result.{AtK, Engagements, GroundTruth, Relevancy, ResultLabels, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCount, TruePositiveCount}

sealed trait Labeled[+A] {
  import Labeled.{WithLabels, WithGroundTruth}

  def results: A

  def map[B](f: A => B): Labeled[B] = this match {
    case WithGroundTruth(results, relevant) => WithGroundTruth(f(results), relevant)
    case WithLabels(results, labels)        => WithLabels(f(results), labels)
  }
}
object Labeled extends WithLabels0 {

  case class WithGroundTruth[+A](results: A, groundTruth: NonEmptySet[ResultId]) extends Labeled[A]
  case class WithLabels[+A](results: A, labels: NonEmptyMap[ResultId, Double]) extends Labeled[A]

  import com.adrielc.quivr.metrics.implicits._
  import cats.implicits._


  object WithGroundTruth extends WithGroundTruth0 {

    def apply[A: GroundTruth](results: A): WithGroundTruth[A] =
      WithGroundTruth(results, results.groundTruth.set)

    def fromLabels[A, V](a: A, labels: Map[ResultId, V], judgeLabel: V => Boolean): Option[WithGroundTruth[A]] =
      labels.toList
        .mapFilter { case (id, v) => judgeLabel(v).guard[Option].as(id) }
        .toNel.map(n => WithGroundTruth(a, n.toNes))

    def fromEngagements[A: Engagements[*, E], E](a: A, judgeLabel: Map[E, Int] => Boolean): Option[WithGroundTruth[A]] =
      WithGroundTruth.fromLabels(a, a.engagements, judgeLabel)

    def fromResultLabels[A: ResultLabels](a: A, judgeLabel: Double => Boolean): Option[WithGroundTruth[A]] =
      WithGroundTruth.fromLabels(a, a.resultLabels.toSortedMap, judgeLabel)

    implicit def withRelGroundTruthSet[A]: GroundTruth[WithGroundTruth[A]] = a => GroundTruth.Set(a.groundTruth)

    implicit def resultsWithRelevant[A: AtK]: AtK[WithGroundTruth[A]] = (a, k) => a.results.atK(k).map(WithGroundTruth(_, a.groundTruth))

    implicit def withRelResultSet[A: Results]: Results[WithGroundTruth[A]] = _.results.results

    implicit def withRelEngagements[A: Engagements[*, E], E]: Engagements[WithGroundTruth[A], E] = _.results.engagements

    implicit val semigroupKWithGroundTruth: SemigroupK[WithGroundTruth] = new SemigroupK[WithGroundTruth] {
      def combineK[A](x: WithGroundTruth[A], y: WithGroundTruth[A]): WithGroundTruth[A] =
        WithGroundTruth(x.results, x.groundTruth union y.groundTruth)
    }
  }

  trait WithGroundTruth0 {

    implicit def withRelNResultSet[A: NonEmptyResults]: NonEmptyResults[WithGroundTruth[A]] = _.results.nonEmptyResults
  }

  object WithLabels extends {

    def apply[A: ResultLabels](results: A): WithLabels[A] =
      WithLabels(results, results.resultLabels)

    def fromGroundTruth[A: GroundTruth](a: A, groundTruthLabel: Double): WithLabels[A] =
      WithLabels(a, a.groundTruth.set.map(_ -> groundTruthLabel).toNonEmptyList.toNem)

    def fromLabels[A, V](a: A, resultLabels: Map[ResultId, V], toLabel: V => Option[Double]): Option[WithLabels[A]] =
      resultLabels.toList
        .mapFilter { case (id, e) => toLabel(e).map(id -> _) }.toNel
        .map(n => WithLabels(a, n.toNem))

    def fromEngagements[A: Engagements[*, E], E](a: A, toLabel: Map[E, Int] => Option[Double]): Option[WithLabels[A]] =
      WithLabels.fromLabels(a, a.engagements, toLabel)

    implicit def withEngagementsInstance[A]: ResultLabels[WithLabels[A]] = _.labels

    implicit def resultsWithLabels[A: AtK]: AtK[WithLabels[A]] = (a, k) => a.results.atK(k).map(WithLabels(_, a.labels))

    implicit def withLabelsResultSetInstance[A: Results]: Results[WithLabels[A]] = _.results.results

    implicit def withLabGroundTruthSet[A: GroundTruth]: GroundTruth[WithLabels[A]] = _.results.groundTruth

    implicit def withLabEngagements[A: Engagements[*, E], E]: Engagements[WithLabels[A], E] = _.results.engagements

    implicit val semigroupKLabels: SemigroupK[WithLabels] = new SemigroupK[WithLabels] {
      def combineK[A](x: WithLabels[A], y: WithLabels[A]): WithLabels[A] =
        WithLabels(x.results, NonEmptyMap.fromMapUnsafe(x.labels.toSortedMap |+| y.labels.toSortedMap))
    }
  }


  implicit val labeledFunctor: Functor[Labeled] = new Functor[Labeled] {
    def map[A, B](fa: Labeled[A])(f: A => B): Labeled[B] = fa.map(f)
  }

  implicit def atKLabeled[A: AtK]: AtK[Labeled[A]] = (a, k) => a match {
    case g: WithGroundTruth[A]  => g.atK(k)
    case l: WithLabels[A]       => l.atK(k)
  }

  implicit def relevanceCount[A: RelevanceCount, F[a] <: Labeled[a]]: RelevanceCount[F[A]] = new RelevanceCount[F[A]] {
    override def groundTruthCount(a: F[A]): Int = a.results.groundTruthCount

    override def truePositiveCount(a: F[A]): Int = a.results.truePositiveCount

    override def resultCount(a: F[A]): Int = RelevanceCount[A].resultCount(a.results)
  }
}


trait WithLabels0 extends WithLabels1 {

  implicit def truePositiveCount[A: TruePositiveCount, F[a] <: Labeled[a]]: TruePositiveCount[F[A]] = new TruePositiveCount[F[A]] {
    lazy val R: TruePositiveCount[A] = TruePositiveCount[A]
    override def truePositiveCount(a: F[A]): Int = R.truePositiveCount(a.results)
    override def resultCount(a: F[A]): Int = R.resultCount(a.results)
  }

  implicit def withLabelsNResultSetInstance[A: NonEmptyResults, F[a] <: Labeled[a]]: NonEmptyResults[F[A]] = new NonEmptyResults[F[A]] {
    override def nonEmptyResults(a: F[A]): NonEmptyVector[ResultId] = NonEmptyResults[A].nonEmptyResults(a.results)
  }
}

trait WithLabels1 {

  implicit def resultRelevancies[A: ResultRelevancies, F[a] <: Labeled[a]]: ResultRelevancies[F[A]] = new ResultRelevancies[F[A]] {
    lazy val R: ResultRelevancies[A] = ResultRelevancies[A]
    override type Rel = R.Rel
    override def rel: Relevancy[R.Rel] = R.rel
    override def resultRelevancies(a: F[A]): NonEmptyVector[R.Rel] = R.resultRelevancies(a.results)
  }
}