package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import cats.implicits.{none, toContravariantOps}
import com.adrielc.quivr.metrics.retrieval.GroundTruthCount
import simulacrum.{op, typeclass}

object result {

  @typeclass trait AtK[A] {

    @op("atK")
    def atK(a: A, k: Int): Option[A]
  }

  object AtK {

    implicit def toKReseults[A]: AtK[NonEmptyList[A]] =
      (a, k) => if(k > a.length) none else NonEmptyList.fromList(a.toList.take(k))

    implicit def toKVectorReseults[A]: AtK[NonEmptyVector[A]] =
      (a, k) => if(k > a.length) none else NonEmptyVector.fromVector(a.toVector.take(k))

    implicit def toLeftTupleAtK[A: AtK, B]: AtK[(A, B)] = (ab, k) => AtK[A].atK(ab._1, k).map(_ -> ab._2)
  }

  @typeclass trait Results[A] extends retrieval.ResultCount[A] {

    @op("results")
    def results(a: A): NonEmptyVector[ResultId]

    def judgeWith(a: A, groundTruth: NonEmptySet[ResultId]): NonEmptyVector[Boolean] =
      results(a).map(groundTruth.contains)

    def labelWith(a: A, labels: NonEmptyMap[ResultId, Label]): NonEmptyVector[Option[Label]] =
      results(a).map(labels.lookup)

    def labeledResults(a: A)(implicit R: ResultLabels[A]): NonEmptyVector[Option[Label]] = {
      val labels = R.resultLabels(a)
      results(a).map(labels.lookup)
    }

    def judgedResults(a: A)(implicit G: GroundTruth[A]): NonEmptyVector[Boolean] = {
      val rels = G.groundTruth(a).set
      results(a).map(rels.contains)
    }

    override def resultCount(a: A): Int =
      results(a).length
  }
  object Results {

    implicit val resultSetIdentityInstance: Results[NonEmptyVector[ResultId]] = identity

    implicit val resultSetNelInstance: Results[NonEmptyList[ResultId]] = nel => NonEmptyVector(nel.head, nel.tail.toVector)

    implicit def resultsLeftTuple[A: Results, B]: Results[(A, B)] = a => Results[A].results(a._1)

    implicit val contravariantResultSet: Contravariant[Results] = new Contravariant[Results] {
      def contramap[A, B](fa: Results[A])(f: B => A): Results[B] = a => fa.results(f(a))
    }
  }

  @typeclass trait ResultLabels[A] extends Serializable {

    def resultLabels(a: A): NonEmptyMap[ResultId, Label]
  }
  object ResultLabels {

    implicit val resultLabelsIdentityInstance: ResultLabels[NonEmptyMap[ResultId, Label]] = identity

    implicit def resultLabelsRTuple[A, B](implicit RB: ResultLabels[B]): ResultLabels[(A, B)] = RB.contramap(_._2)

    implicit val contravariantResultLabels: Contravariant[ResultLabels] = new Contravariant[ResultLabels] {
      def contramap[A, B](fa: ResultLabels[A])(f: B => A): ResultLabels[B] = a => fa.resultLabels(f(a))
    }
  }

  trait Engagements[A, E] {

    def engagements(a: A): Map[ResultId, Map[E, Int]]
  }

  object Engagements {
    def apply[A, E](implicit E: Engagements[A, E]): Engagements[A, E] = E

    trait ToEngagementsOps {
      implicit def toEngagementsOps[A](a: A): EngagementsOps[A] = new EngagementsOps(a)
    }
    implicit class EngagementsOps[A](private val a: A) extends AnyVal {
      def engagementCounts[E](implicit E: Engagements[A, E]): Map[ResultId, Map[E, Int]] = E.engagements(a)
    }

    implicit def engagementsIdentityInstance[E]: Engagements[Map[ResultId, Map[E, Int]], E] = identity

    implicit def engagementsRightTuple[A, B: Engagements[*, E], E]: Engagements[(A, B), E] = ab => Engagements[B, E].engagements(ab._2)

    implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
      def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagements(f(a))
    }
  }

  @typeclass trait GroundTruth[A] extends GroundTruthCount[A] {

    def groundTruth(a: A): GroundTruth.RelSet

    override def groundTruthCount(a: A): Int =
      groundTruth(a).nRel
  }
  object GroundTruth {

    // relevant document set
    case class RelSet(set: NonEmptySet[ResultId]) {

      lazy val nRel: Int = set.length
    }

    implicit val contravariantGroundTruthSet: Contravariant[GroundTruth] = new Contravariant[GroundTruth] {
      def contramap[A, B](fa: GroundTruth[A])(f: B => A): GroundTruth[B] = a => fa.groundTruth(f(a))
    }
  }
}
