package com.adrielc.quivr.metrics
package ranking

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyVector}
import com.adrielc.quivr.metrics.data.Rankings.Ranked
import com.adrielc.quivr.metrics.result.{BinaryRelevancy, GroundTruth, Relevancy, ResultLabels, Results}
import com.adrielc.quivr.metrics.retrieval.ResultCount
import simulacrum.typeclass
import eu.timepit.refined.auto._

/**
 * Represents a complete list of results with their associated relevanceies
 *
 * @tparam A
 */
@typeclass trait ResultRelevancies[A] extends RankedRelevancies[A] with ResultCount[A] {

  type Rel

  implicit def relevancy: Relevancy[Rel]

  def resultRelevancies(a: A): NonEmptyVector[Rel]

  override def resultCount(a: A): Int =
    resultRelevancies(a).length

  override def rankedRelevancies(a: A): Ranked[Rel] =
    Ranked(resultRelevancies(a))
}

object ResultRelevancies {
  type Aux[A, R] = ResultRelevancies[A] { type Rel = R }

  def instance[A, R: Relevancy](rels: A => NonEmptyVector[R]): ResultRelevancies.Aux[A, R] = new ResultRelevancies[A] {
    override type Rel = R
    override val relevancy: Relevancy[R] = Relevancy[R]
    override def resultRelevancies(a: A): NonEmptyVector[Rel] = rels(a)
  }

  implicit def identityLabelledSet[R: Relevancy]: ResultRelevancies.Aux[NonEmptyVector[R], R] = instance(identity)

  implicit def nelLabelledSet[R: Relevancy]: ResultRelevancies.Aux[NonEmptyList[R], R] = instance(nel => NonEmptyVector(nel.head, nel.tail.toVector))

  implicit def contravariantRels[R]: Contravariant[ResultRelevancies.Aux[*, R]] = new Contravariant[ResultRelevancies.Aux[*, R]] {
    def contramap[A, B](fa: ResultRelevancies.Aux[A, R])(f: B => A): ResultRelevancies.Aux[B, R] = new ResultRelevancies[B] {
      override type Rel = R
      override val relevancy: Relevancy[R] = fa.relevancy
      def resultRelevancies(a: B): NonEmptyVector[Rel] = fa.resultRelevancies(f(a))
    }
  }

  implicit def relevancyLabelsFromGroundTruth[S: Results : ResultLabels]: ResultRelevancies.Aux[S, Option[Double]] = new ResultRelevancies[S] {
    override type Rel = Option[Double]
    override val relevancy: Relevancy[Rel] = implicitly
    def resultRelevancies(a: S): NonEmptyVector[Rel] = {
      val res = Results[S].results(a)
      val labs = ResultLabels[S].resultLabels(a)
      res.map(id => labs.lookup(id))
    }
  }
}

@typeclass trait ResultJudgements[A] extends ResultRelevancies[A] with RankedJudgements[A] {
  import result.AtK.ops._
  import retrieval.TruePositiveCount.ops._

  type Rel

  implicit def relevancy: BinaryRelevancy[Rel]

  def precisionAtK(a: A, k: Rank): Option[Double] =
    resultRelevancies(a).atK(k).flatMap(_.precision)

  def recallAtK(a: A, k: Rank): Option[Double] = {
    val rels = resultRelevancies(a)
    val countRel = rels.toVector.count(relevancy.isRel)
    rels.atK(k).flatMap { rels =>
      safeDiv(rels.toVector.count(relevancy.isRel).toDouble, countRel.toDouble)
    }
  }

  def fScoreAtK(a: A, k: Rank): Option[Double] =
    for {
      r <- recallAtK(a, k)
      p <- precisionAtK(a, k)
      plus = r + p
      if plus != 0
    } yield (2 * (r * p)) / plus

  def rPrecision(a: A): Option[Double] =
    Rank.from(truePositiveCount(a)).toOption
      .flatMap(precisionAtK(a, _))

  override def resultCount(a: A): Int =
    resultRelevancies(a).length

  override def rankedRelevancies(a: A): Ranked[Rel] =
    Ranked(resultRelevancies(a))

  override def truePositiveCount(a: A): Int =
    resultRelevancies(a).toVector.count(relevancy.isRel)
}

object ResultJudgements {
  type Aux[A, R] = ResultJudgements[A] {type Rel = R}

  def instance[A, R: BinaryRelevancy](rels: A => NonEmptyVector[R]): ranking.ResultJudgements.Aux[A, R] = new ResultJudgements[A] {
    override type Rel = R
    override val relevancy: BinaryRelevancy[R] = BinaryRelevancy[R]
    override def resultRelevancies(a: A): NonEmptyVector[Rel] = rels(a)
  }

  implicit def identityLabelledSet[R: BinaryRelevancy]: ResultJudgements.Aux[NonEmptyVector[R], R] = instance(identity)

  implicit def binaryJudgementsFromGroundTruth[S: Results : GroundTruth]: ResultJudgements.Aux[S, Boolean] = new ResultJudgements[S] {
    override type Rel = Boolean
    override val relevancy: BinaryRelevancy[Rel] = implicitly[BinaryRelevancy[Rel]]

    def resultRelevancies(a: S): NonEmptyVector[Rel] = {
      val res = Results[S].results(a)
      val groundTruth = GroundTruth[S].groundTruth(a).set
      res.map(id => groundTruth.contains(id))
    }

    override def truePositiveCount(a: S): Int = {
      val rels = GroundTruth[S].groundTruth(a).set
      Results[S].results(a).toVector.count(rels.contains)
    }
  }
}
