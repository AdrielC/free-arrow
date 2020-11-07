package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyVector}
import com.adrielc.quivr.metrics.data.Rankings.Ranked
import simulacrum.{op, typeclass}
import cats.implicits._
import eu.timepit.refined.auto._
import function._
import com.adrielc.quivr.metrics.data.relevance.{Relevance, SABRel}
import com.adrielc.quivr.metrics.result.{GroundTruth, ResultLabels, Results}
import com.adrielc.quivr.metrics.retrieval.TruePositiveCount

object ranking {

  @typeclass trait Relevancy[Rel] {
    def gainValue(r: Rel): Option[Gain]
    def isJudged(a: Rel): Boolean = gainValue(a).isDefined
    def isRel(a: Rel): Boolean = gainValue(a).exists(_ > 0)
    def gainOrZero(r: Rel): Double = gainValue(r).getOrElse(0.0)
  }
  object Relevancy {

    implicit val gainRelevancy: Relevancy[Option[Gain]] = identity

    implicit val relevancyOptBool: Relevancy[Option[Boolean]] = _.flatMap(if(_) Some(1.0) else None)

    implicit def relevancyOptNum[N](N: Numeric[N]): Relevancy[Option[N]] = _.map(N.toDouble)

    implicit def relevancySABRel: Relevancy[SABRel] = _.gain

    implicit val relevanceRelevancy: Relevancy[Relevance] = _.gain

    implicit def relevancyNonOpt[A](implicit R: Relevancy[Option[A]]): Relevancy[A] = R.contramap(Option(_))

    implicit val contravariant: Contravariant[Relevancy] = new Contravariant[Relevancy] {
      def contramap[A, B](fa: Relevancy[A])(f: B => A): Relevancy[B] = a => fa.gainValue(f(a))
    }
  }

  /**
   * Represents a possibly discontiguous ranked list of relevance labels
   * @tparam A
   */
  @typeclass trait RankedRelevancies[A] extends Serializable {
    import implicits._

    type Rel

    implicit def rel: Relevancy[Rel]

    def rankedRelevancies(a: A): Ranked[Rel]

    def ndcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

    def dcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

    @op("avgPrec", alias = true)
    def averagePrecision(a: A): Option[Double] =
      calcAP(rankedRelevancies(a))

    @op("avgPrecK", alias = true)
    def averagePrecisionK(a: A, k: Rank): Option[Double] =
      gains(a).atK(k).flatMap(a => calcAP(a))

    //    Because BR(r) has an r in the denominator (just like P(r)),
    //    Q-measure is guaranteed to become smaller as a relevant document goes
    //    down the ranked list. A large b (e.g., b = 100) alleviates this effect,
    //    and makes Q-measure more forgiving for relevant documents near the bottom of the ranked list.
    //    Conversely, a small b (e.g., b = 1) imposes more penalty
    def qMeasure(a: A, b: Double = 1): Option[Double] =
      calcQ(rankedRelevancies(a), b)


    def qMeasureK(a: A, k: Rank, b: Double = 1): Option[Double] =
      gains(a).atK(k).flatMap(calcQ(_, b))

    def reciprocalRank(a: A): Option[Double] =
      calcReciprocalRank(rankedRelevancies(a).rankings)

    private def gains(a: A): Ranked[Double] =
      rankedRelevancies(a).map(rel.gainOrZero)
  }
  object RankedRelevancies {
    type Aux[A, R] = RankedRelevancies[A] { type Rel = R }
    implicit def fromRelevancies[A: ResultRelevancies]: ResultRelevancies[A] = ResultRelevancies[A]
  }


  /**
   * Represents a complete list of results with their associated relevanceies
   * @tparam A
   */
  @typeclass trait ResultRelevancies[A] extends RankedRelevancies[A] with TruePositiveCount[A] {
    import implicits._

    type Rel

    implicit def rel: Relevancy[Rel]

    def resultRelevancies(a: A): NonEmptyVector[Rel]

    final def dcg(a: A, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Double =
      calcDcg(gains(a), g, d)

    final def ndcg(a: A, g: GainFn = gain.pow2, d: DiscountFn = discount.log2): Option[Double] =
      calcNdcg(gains(a), g, d)

    def precisionAtK(a: A, k: Rank): Option[Double] =
      resultRelevancies(a).atK(k).flatMap(_.precision)

    def recallAtK(a: A, k: Rank): Option[Double] = {
      val rels = resultRelevancies(a)
      val countRel = rels.toVector.count(rel.isRel)
      resultRelevancies(a).atK(k).flatMap { rels =>
        safeDiv(rels.toList.count(rel.isRel).toDouble, countRel.toDouble)
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

    private def gains(a: A): NonEmptyVector[Double] =
      resultRelevancies(a).map(_.gainOrZero)

    override def truePositiveCount(a: A): Int =
      resultRelevancies(a).toList.count(_.isRel)
  }
  object ResultRelevancies {
    type Aux[A, R] = ResultRelevancies[A] { type Rel = R }

    def instance[A, R: Relevancy](rels: A => NonEmptyVector[R]): ResultRelevancies.Aux[A, R] = new ResultRelevancies[A] {
      override type Rel = R
      override val rel: Relevancy[R] = Relevancy[R]
      override def resultRelevancies(a: A): NonEmptyVector[Rel] = rels(a)
    }

    implicit def identityLabelledSet[R: Relevancy]: ResultRelevancies.Aux[NonEmptyVector[R], R] = instance(identity)

    implicit def nelLabelledSet[R: Relevancy]: ResultRelevancies.Aux[NonEmptyList[R], R] = instance(nel => NonEmptyVector(nel.head, nel.tail.toVector))

    implicit def binaryJudgementsFromGroundTruth[S: Results: GroundTruth : ResultLabels]: ResultRelevancies.Aux[S, Option[Label]] = new ResultRelevancies[S] {
      type Rel = Option[Label]
      override val rel: Relevancy[Option[Label]] = Relevancy[Option[Label]]
      def resultRelevancies(a: S): NonEmptyVector[Rel] = Results[S].labeledResults(a)
      override def truePositiveCount(a: S): Int = {
        val rels = GroundTruth[S].groundTruth(a).set
        Results[S].results(a).toVector.count(rels.contains)
      }
    }

    implicit def contravariantRels[R]: Contravariant[ResultRelevancies.Aux[*, R]] = new Contravariant[ResultRelevancies.Aux[*, R]] {
      def contramap[A, B](fa: ResultRelevancies.Aux[A, R])(f: B => A): ResultRelevancies.Aux[B, R] = new ResultRelevancies[B] {
        override type Rel = R
        override val rel: Relevancy[R] = fa.rel
        def resultRelevancies(a: B): NonEmptyVector[Rel] = fa.resultRelevancies(f(a))
      }
    }
  }
}
