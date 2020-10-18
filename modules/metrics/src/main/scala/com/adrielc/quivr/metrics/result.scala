package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits.none
import com.adrielc.quivr.metrics.retrieval.{GroundTruthCount, ResultCount}
import com.adrielc.quivr.metrics.data.{KeyCounts, Label, Rank, ResultId}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Judged.WithLabels
import eu.timepit.refined.auto._
import simulacrum.{op, typeclass}


object result {

  @typeclass trait AtK[A] {

    @op("atK")
    def atK(a: A, k: Rank): Option[A]
  }

  object AtK {

    implicit def toKReseults[A]: AtK[NonEmptyList[A]] =
      (a, k) => if(k > a.length) none else a.toList.take(k).toNel
  }


  @typeclass trait Results[A] extends ResultCount[A] {

    @op("results")
    def results(a: A): NonEmptyList[ResultId]

    def judgeWith(a: A, groundTruth: NonEmptySet[ResultId]): NonEmptyList[Boolean] =
      results(a).map(groundTruth.contains)

    def labelWith(a: A, labels: NonEmptyMap[ResultId, Label]): NonEmptyList[Double] =
      results(a).map(labels.lookup(_).getOrElse(0.0))

    override def resultCount(a: A): Int =
      results(a).size
  }
  object Results {
    implicit val resultSetIdentityInstance: Results[NonEmptyList[ResultId]] = identity
    implicit val contravariantResultSet: Contravariant[Results] = new Contravariant[Results] {
      def contramap[A, B](fa: Results[A])(f: B => A): Results[B] = a => fa.results(f(a))
    }
  }


  trait Engagements[A, E] {

    def engagementCounts(a: A): Map[ResultId, KeyCounts[E]]
  }

  object Engagements {
    trait ToEngagementsOps {
      implicit def toEngagementsOps[A](a: A): EngagementsOps[A] = new EngagementsOps(a)
    }
    class EngagementsOps[A](private val a: A) extends AnyVal {
      def engagementCounts[E](implicit E: Engagements[A, E]): Map[ResultId, KeyCounts[E]] = E.engagementCounts(a)
    }
    implicit def engagementsIdentityInstance[E]: Engagements[Map[ResultId, KeyCounts[E]], E] = identity
    implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
      def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagementCounts(f(a))
    }
  }

  @typeclass trait ResultLabels[A] extends Serializable {

    @op("resultLabels")
    def resultLabels(a: A): NonEmptyMap[ResultId, Label]
  }
  object ResultLabels {
    implicit val resultLabelsIdentityInstance: ResultLabels[NonEmptyMap[ResultId, Label]] = identity
    implicit val contravariantResultLabels: Contravariant[ResultLabels] = new Contravariant[ResultLabels] {
      def contramap[A, B](fa: ResultLabels[A])(f: B => A): ResultLabels[B] = a => fa.resultLabels(f(a))
    }
  }


  @typeclass trait GroundTruthSet[A] extends GroundTruthCount[A] {

    @op("groundTruthSet")
    def groundTruthSet(a: A): NonEmptySet[ResultId]

    @op("withLabelsFromGroundTruth")
    def withLabelsFromGroundTruth(a: A, groundTruthLabel: Label): WithLabels[A] =
      WithLabels(a, groundTruthSet(a).map(_ -> groundTruthLabel).toNonEmptyList.toNem)

    override def groundTruthCount(a: A): Int =
      groundTruthSet(a).length
  }
  object GroundTruthSet {
    implicit val identityGroundTruthSetInstance: GroundTruthSet[NonEmptySet[ResultId]] = identity
    implicit val contravariantGroundTruthSet: Contravariant[GroundTruthSet] = new Contravariant[GroundTruthSet] {
      def contramap[A, B](fa: GroundTruthSet[A])(f: B => A): GroundTruthSet[B] = a => fa.groundTruthSet(f(a))
    }
  }
}
