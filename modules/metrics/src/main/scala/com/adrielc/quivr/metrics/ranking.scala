package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.retrieval.TruePositiveCount
import com.adrielc.quivr.metrics.data.{Gain, Label, Rank, Ranked}
import com.adrielc.quivr.metrics.result.{Qrels, ResultLabels, Results}
import simulacrum.{op, typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.ranking.Relevancies.identityLabelledSet
import com.adrielc.quivr.metrics.relevancy.Relevancy
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._

object ranking {
  import function._

  @typeclass trait PartialRelevancies[A] extends Serializable {

    type Rel

    implicit def rel: Relevancy[Rel]

    def partialRelevanceLabels(a: A): Ranked[Rel]

    // Return a list of all relevant ranks with their associated non-zero gain value
    def condensedList(a: A): Option[Ranked[(Rel, Gain)]] = {
      val labs = partialRelevanceLabels(a)
      partialRelevanceLabels(a).indexes.toNel.toList
        .mapFilter { case (r, res) => res.gainValue.map(g => (r, (res, g))) }.toNel
        .map(nel => Ranked(nel.toNem, labs.k))
    }

    def ndcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: Discount = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Gain], g, d))

    def dcgK(a: A, k: Rank, g: GainFn = gain.pow2, d: Discount = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Gain], g, d))

    @op("avgPrec", alias = true)
    def averagePrecision(a: A): Option[Double] = {
      val rnk = partialRelevanceLabels(a)
      val (correct, sum) = rnk.indexes.toNel.foldLeft((0, 0.0)) { case ((c, s), (i, r)) =>
        if (r.isRel) {
          val correct = c + 1
          val sum = s + (correct / i.toDouble)
          (correct, sum)
        } else (c, s)
      }
      safeDiv(sum.toDouble, correct.toDouble)
    }

    @op("avgPrecK", alias = true)
    def averagePrecisionK(a: A, k: Rank): Option[Double] =
      gains(a).atK(k).flatMap(_.averagePrecision)


    //    Because BR(r) has an r in the denominator (just like P(r)),
    //    Q-measure is guaranteed to become smaller as a relevant document goes
    //    down the ranked list. A large b (e.g., b = 100) alleviates this effect,
    //    and makes Q-measure more forgiving for relevant documents near the bottom of the ranked list.
    //    Conversely, a small b (e.g., b = 1) imposes more penalty
    def qMeasure(a: A, b: Double = 1): Option[Double] = {
      val rnk = partialRelevanceLabels(a)
      val labs = rnk.indexes.toNel
      val ideal = labs.sortBy(-_._2.gainOrZero)
      val withIdeal = labs.zipWith(ideal)((a, b) => (Rank.fromIndex(a._1), a._2, b._2))
      val (_, _, correct, sum) = withIdeal.foldLeft((0.0, 0.0, 0, 0.0)) {
        case (prev @ (cg, cgI, c, s), (k, r, rIdeal)) =>
          if(r.isRel) {
            val cG      = cg + r.gainOrZero
            val cGI     = cgI + rIdeal.gainOrZero
            val correct = c + 1
            val sum     = s + ((b*cG + correct) / (b*cGI + k.toDouble))
            (cG, cGI, correct, sum)
          } else prev
      }
      safeDiv(sum.toDouble, correct.toDouble)
    }

    def qMeasureK(a: A, k: Rank, b: Double = 1): Option[Double] =
      gains(a).atK(k).flatMap(_.qMeasure(b))

    def reciprocalRank(a: A): Option[Double] =
      partialRelevanceLabels(a).indexes.toNel
        .find { case (_, r) => r.isRel }
        .map { case (k, _) => 1 / k.toDouble }

    private def gains(a: A): Ranked[Gain] =
      partialRelevanceLabels(a).map(_.gainOrZero)
  }
  object PartialRelevancies {
    type Aux[A, R] = PartialRelevancies[A] { type Rel = R }
    implicit def fromRelevancies[A: Relevancies]: PartialRelevancies[A] = Relevancies[A]
  }


  /**
   *
   * @tparam A A complete list of results with their associated relevancies
   *           Their position in the list
   *
   */
  @typeclass trait Relevancies[A] extends PartialRelevancies[A] with TruePositiveCount[A] {

    type Rel

    def relevancies(a: A): NonEmptyList[Rel]

    final def dcg(a: A, g: GainFn = gain.pow2, d: Discount = discount.log2): Double =
      calcDcg(gains(a), g, d)

    final def ndcg(a: A, g: GainFn = gain.pow2, d: Discount = discount.log2): Option[Double] =
      calcNdcg(gains(a), g, d)

    def precisionAtK(a: A, k: Rank): Option[Double] =
      relevancies(a).atK(k).flatMap(_.precision)

    def recallAtK(a: A, k: Rank): Option[Double] = {
      val rels = relevancies(a)
      val countRel = rels.count(_.isRel)
      relevancies(a).atK(k).flatMap { rels =>
        safeDiv(rels.toList.count(_.isRel).toDouble, countRel.toDouble)
      }
    }

    def fScoreAtK(a: A, k: Rank): Option[Double] =
      for {
        r <- recallAtK(a, k)
        p <- precisionAtK(a, k)
        plus = r + p
        if plus != 0
      } yield (2 * (r * p)) / plus

    def rPrecision(a: A): Option[Double] = {
      val judgements = relevancies(a)
      val nRel = judgements.toList.count(_.isRel)
      judgements.toList.take(nRel).toNel.flatMap(_.precision)
    }

    override def resultCount(a: A): Int =
      relevancies(a).length

    override def partialRelevanceLabels(a: A): Ranked[Rel] =
      Ranked(relevancies(a))

    private def gains(a: A): NonEmptyList[Gain] =
      relevancies(a).map(_.gainOrZero)

    override def truePositiveCount(a: A): Int =
      relevancies(a).count(_.isRel).toInt
  }
  object Relevancies extends Relevancies0 {
    type Aux[A, R] = Relevancies[A] { type Rel = R }
    implicit def identityLabelledSet[R: Relevancy]: Relevancies.Aux[NonEmptyList[R], R] = new Relevancies[NonEmptyList[R]] {
      type Rel = R
      override val rel: Relevancy[R] = Relevancy[R]
      def relevancies(a: NonEmptyList[R]): NonEmptyList[Rel] = a
    }
    implicit def binaryJudgementsFromGroundTruth[S: Results: Qrels : ResultLabels]: Relevancies.Aux[S, Option[Label]] = new Relevancies[S] {
      type Rel = Option[Label]
      override val rel: Relevancy[Option[Label]] = Relevancy[Option[Label]]
      def relevancies(a: S): NonEmptyList[Rel] = a.labelWith(a.resultLabels)
      override def truePositiveCount(a: S): Int = a.results.count(a.qrels.set.contains).toInt
    }

    implicit def contravariantRels[R]: Contravariant[Relevancies.Aux[*, R]] = new Contravariant[Relevancies.Aux[*, R]] {
      def contramap[A, B](fa: Relevancies.Aux[A, R])(f: B => A): Relevancies.Aux[B, R] = new Relevancies[B] {
        override type Rel = R
        override val rel: Relevancy[R] = fa.rel
        def relevancies(a: B): NonEmptyList[Rel] = fa.relevancies(f(a))
      }
    }
  }
  trait Relevancies0 extends Relevancies1 {
    implicit def labelledSetFromResultsInstance[S: Results: ResultLabels]: Relevancies.Aux[S, Option[Label]] =
      Relevancies.contravariantRels.contramap(identityLabelledSet[Option[Label]])(r => r.labelWith(r.resultLabels))
  }
  trait Relevancies1 {
    implicit def binarySetFromResultsInstance[S: Results: Qrels]: Relevancies.Aux[S, Boolean] =
      Relevancies.contravariantRels.contramap(identityLabelledSet[Boolean])(r => r.judgeWith(r.qrels.set))
  }
}
