package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits.none
import com.adrielc.quivr.metrics.retrieval.{GroundTruthCount, ResultCount}
import com.adrielc.quivr.metrics.data.{Label, Rank, ResultId}
import simulacrum.{op, typeclass}

import cats.implicits._
import eu.timepit.refined.auto._


object result {

  @typeclass trait AtK[A] {

    @op("atK")
    def atK(a: A, k: Rank): Option[A]
  }

  object AtK {

    implicit def toKReseults[A]: AtK[NonEmptyList[A]] =
      (a, k) => if(k > a.length) none else a.toList.take(k).toNel

    implicit def toLeftTupleAtK[A: AtK, B]: AtK[(A, B)] = (ab, k) => ab._1.atK(k).map(_ -> ab._2)
  }


  @typeclass trait Results[A] extends ResultCount[A] {

    @op("results")
    def results(a: A): NonEmptyList[ResultId]

    def judgeWith(a: A, groundTruth: NonEmptySet[ResultId]): NonEmptyList[Boolean] =
      results(a).map(groundTruth.contains)

    def labelWith(a: A, labels: NonEmptyMap[ResultId, Label]): NonEmptyList[Option[Label]] =
      results(a).map(labels.lookup)

    override def resultCount(a: A): Int =
      results(a).size
  }
  object Results {
    implicit val resultSetIdentityInstance: Results[NonEmptyList[ResultId]] = identity
    implicit def resultsLeftTuple[A: Results, B]: Results[(A, B)] = _._1.results
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
    implicit def engagementsRightTuple[A, B: Engagements[*, E], E]: Engagements[(A, B), E] = _._2.engagementCounts
    implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
      def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagements(f(a))
    }
  }

  trait EngagedResults[A, E] extends Results[A] with Engagements[A, E]
  object EngagedResults {
    implicit def fromResEng[A, E](implicit E: Engagements[A, E], R: Results[A]): EngagedResults[A, E] = new EngagedResults[A, E] {
      override def results(a: A): NonEmptyList[ResultId] = R.results(a)
      override def engagements(a: A): Map[ResultId, Map[E, Int]] = E.engagements(a)
    }
  }

  @typeclass trait ResultLabels[A] extends Serializable {

    @op("resultLabels")
    def resultLabels(a: A): NonEmptyMap[ResultId, Label]
  }
  object ResultLabels {
    implicit val resultLabelsIdentityInstance: ResultLabels[NonEmptyMap[ResultId, Label]] = identity
    implicit def resultLabelsRTuple[A, B: ResultLabels]: ResultLabels[(A, B)] = _._2.resultLabels
    implicit val contravariantResultLabels: Contravariant[ResultLabels] = new Contravariant[ResultLabels] {
      def contramap[A, B](fa: ResultLabels[A])(f: B => A): ResultLabels[B] = a => fa.resultLabels(f(a))
    }
  }


  @typeclass trait GroundTruth[A] extends GroundTruthCount[A] {

    def groundTruth(a: A): GroundTruth.RelSet

    override def groundTruthCount(a: A): Int =
      groundTruth(a).nRel
  }
  object GroundTruth {
    implicit val contravariantGroundTruthSet: Contravariant[GroundTruth] = new Contravariant[GroundTruth] {
      def contramap[A, B](fa: GroundTruth[A])(f: B => A): GroundTruth[B] = a => fa.groundTruth(f(a))
    }

    // relevant document set
    case class RelSet(set: NonEmptySet[ResultId]) {

      lazy val nRel: Int = set.length
    }
  }
}
