package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import com.adrielc.quivr.metrics.retrieval.ResultCount
import simulacrum.{op, typeclass}

@typeclass trait Results[A] extends ResultCount[A] {

  @op("results")
  def results(a: A): Vector[ResultId]

  def judgeWith(a: A, groundTruth: NonEmptySet[ResultId]): Vector[Boolean] =
    results(a).map(groundTruth.contains)

  def labelWith(a: A, labels: NonEmptyMap[ResultId, Label]): Vector[Option[Label]] =
    results(a).map(labels.lookup)

  def labeledResults(a: A)(implicit R: ResultLabels[A]): Vector[Option[Label]] = {
    val labels = R.resultLabels(a)
    results(a).map(labels.lookup)
  }

  def judgedResults(a: A)(implicit G: GroundTruth[A]): Vector[Boolean] = {
    val rels = G.groundTruth(a).set
    results(a).map(rels.contains)
  }

  override def resultCount(a: A): Int =
    results(a).length
}
object Results {

  @typeclass trait NonEmptyResults[A] extends Results[A] {

    def nonEmptyResults(a: A): NonEmptyVector[ResultId]

    override def results(a: A): Vector[ResultId] = nonEmptyResults(a).toVector

    def nonEmptyLabeledResults(a: A)(implicit R: ResultLabels[A]): NonEmptyVector[Option[Label]] = {
      val labels = R.resultLabels(a)
      nonEmptyResults(a).map(labels.lookup)
    }
  }
  object NonEmptyResults {

    implicit val resultSetIdentity: NonEmptyResults[NonEmptyVector[ResultId]] = identity

    implicit val resultSetNelInstance: NonEmptyResults[NonEmptyList[ResultId]] = _.toNev

    implicit def resultsLeftTuple[A: NonEmptyResults, B]: NonEmptyResults[(A, B)] = a => NonEmptyResults[A].nonEmptyResults(a._1)

    implicit val contravariantResultSet: Contravariant[NonEmptyResults] = new Contravariant[NonEmptyResults] {
      def contramap[A, B](fa: NonEmptyResults[A])(f: B => A): NonEmptyResults[B] = a => fa.nonEmptyResults(f(a))
    }
  }

  implicit def fromNonEmpty[A: NonEmptyResults]: Results[A] = NonEmptyResults[A]

  implicit def resultSetNelInstance[S <: Seq[ResultId]]: Results[S] = _.toVector

  implicit def resultsLeftTuple[A: Results, B]: Results[(A, B)] = a => Results[A].results(a._1)

  implicit val contravariantResultSet: Contravariant[Results] = new Contravariant[Results] {
    def contramap[A, B](fa: Results[A])(f: B => A): Results[B] = a => fa.results(f(a))
  }
}
