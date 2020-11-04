package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits.none
import com.adrielc.quivr.metrics.retrieval.{GroundTruthCount, ResultCount}
import com.adrielc.quivr.metrics.data.{Label, Rank, ResultId}
import cats.implicits._
import com.adrielc.quivr.metrics.types.QrelSet
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

    def engagements(a: A): Map[ResultId, Map[E, Int]]
  }

  object Engagements {
    trait ToEngagementsOps {
      implicit def toEngagementsOps[A](a: A): EngagementsOps[A] = new EngagementsOps(a)
    }
    class EngagementsOps[A](private val a: A) extends AnyVal {
      def engagementCounts[E](implicit E: Engagements[A, E]): Map[ResultId, Map[E, Int]] = E.engagements(a)
    }
    implicit def engagementsIdentityInstance[E]: Engagements[Map[ResultId, Map[E, Int]], E] = identity
    implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
      def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagements(f(a))
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


  @typeclass trait Qrels[A] extends GroundTruthCount[A] {

    @op("groundTruthSet", alias = true)
    def qrels(a: A): QrelSet


    override def groundTruthCount(a: A): Int =
      qrels(a).nRel
  }
  object Qrels {
    implicit val contravariantGroundTruthSet: Contravariant[Qrels] = new Contravariant[Qrels] {
      def contramap[A, B](fa: Qrels[A])(f: B => A): Qrels[B] = a => fa.qrels(f(a))
    }
  }
}
