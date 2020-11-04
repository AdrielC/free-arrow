package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.retrieval.{ResultCount, TruePositiveCount}
import com.adrielc.quivr.metrics.data.{Ranked, Label, Rank}
import com.adrielc.quivr.metrics.result.{Qrels, ResultLabels, Results}
import simulacrum.{typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.relevancy.Relevancy
import eu.timepit.refined.cats._

object ranking {

  @typeclass trait PartialRelevancy[A] extends Serializable {

    type Rel
    implicit def rel: Relevancy[Rel]
    def partialRelevanceLabels(a: A): Ranked[Rel]

    def condensedList(a: A): Option[Ranked[Rel]] = {
      val labs = partialRelevanceLabels(a)
      partialRelevanceLabels(a).indexes.toNel.toList
        .mapFilter { case (r, res) => res.isJudged.guard[Option].as(r -> res) }.toNel
        .map(nel => Ranked(nel.toNem, labs.k))
    }

    def ndcgK(a: A, k: Rank, g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

    def dcgK(a: A, k: Rank, g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] =
      gains(a).atK(k).flatMap(calcNdcgK(_: Ranked[Double], g, d))

    private def gains(a: A): Ranked[Double] =
      partialRelevanceLabels(a).map(_.gainValue.getOrElse(0.0))
  }

  @typeclass trait GradedRelevance[A] extends PartialRelevancy[A] with ResultCount[A] { self =>

    type Rel = Double

    def relevanceLabels(a: A): NonEmptyList[Double]

    final def dcg(a: A, g: Gain = gain.pow2, d: Discount = discount.log2): Double =
      calcDcg(relevanceLabels(a), g, d)

    final def ndcg(a: A, g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] =
      calcNdcg(relevanceLabels(a), g, d)

    //    Because BR(r) has an r in the denominator (just like P(r)),
    //    Q-measure is guaranteed to become smaller as a relevant document goes
    //    down the ranked list. A large b (e.g., b = 100) alleviates this effect,
    //    and makes Q-measure more forgiving for relevant documents near the bottom of the ranked list.
    //    Conversely, a small b (e.g., b = 1) imposes more penalty
    def qMeasure(a: A, b: Double = 1): Option[Double] = {
      val labs = relevanceLabels(a)
      val ideal = labs.sortBy(-_)
      val withIdeal = labs.zipWith(ideal)((a, b) => (a, b))
      val (_, _, correct, sum) = withIdeal.zipWithIndex.foldLeft((0.0, 0.0, 0, 0.0)) {
        case (prev @ (cg, cgI, c, s), ((rel, relI), i)) =>
          if(rel > 0) {
            val cG      = cg + rel
            val cGI     = cgI + relI
            val correct = c + 1
            val sum     = s + ((b*cG + correct) / (b*cGI + (i+1.0)))
            (cG, cGI, correct, sum)
          } else prev
      }
      safeDiv(sum.toDouble, correct.toDouble)
    }

    override def resultCount(a: A): Int =
      relevanceLabels(a).length

    def asRelevanceJudgements(f: Label => Boolean): BinaryRelevance[A] =
      relevanceLabels(_).map(f)

    override def partialRelevanceLabels(a: A): Ranked[Label] =
      Ranked(relevanceLabels(a))

    def rel: Relevancy[Double] = Relevancy[Double]
  }
  object GradedRelevance {
    implicit val identityLabelledSet: GradedRelevance[NonEmptyList[Label]] = identity
    implicit def labelledSetFromResultsInstance[S: Results: ResultLabels]: GradedRelevance[S] = r => r.labelWith(r.resultLabels)
    implicit val contravariantForLabelledSet: Contravariant[GradedRelevance] = new Contravariant[GradedRelevance] {
      def contramap[A, B](fa: GradedRelevance[A])(f: B => A): GradedRelevance[B] = a => fa.relevanceLabels(f(a))
    }
  }



  @typeclass trait BinaryRelevance[A] extends TruePositiveCount[A] { self =>

    def relevanceJudgements(a: A): NonEmptyList[Boolean]

    def averagePrecision(a: A): Option[Double] = {
      val (correct, sum) = relevanceJudgements(a).zipWithIndex.foldLeft((0, 0.0)) { case ((c, s), (isRel, i)) =>
        if (isRel) {
          val correct = c + 1
          val sum     = s + (correct / (i + 1).toDouble)
          (correct, sum)
        } else (c, s)
      }
      safeDiv(sum.toDouble, correct.toDouble)
    }

    final def binaryNdcg(a: A, g: Gain = gain.pow2, d: Discount = discount.log2): Option[Double] =
      calcNdcg(binaryLabels(a), g, d)

    def reciprocalRank(a: A): Option[Double] =
      relevanceJudgements(a).zipWithIndex
        .find { case (isRel, _) => isRel }
        .map { case (_, idx) => 1 / (idx + 1).toDouble }

    def precisionAtK(a: A, k: Rank): Option[Double] =
      relevanceJudgements(a).atK(k).flatMap(_.precision)

    def recallAtK(a: A, k: Rank): Option[Double] = {
      val rel = relevanceJudgements(a)
      val countRel = rel.count(identity)
      relevanceJudgements(a).atK(k).flatMap { rel =>
        safeDiv(rel.toList.count(identity).toDouble, countRel.toDouble)
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
      val judgements = relevanceJudgements(a)
      val nRel = judgements.toList.count(identity)
      judgements.toList.take(nRel).toNel.flatMap(_.precision)
    }

    def asBinaryLabels(relevanceToLabel: Boolean => Label): GradedRelevance[A] =
      relevanceJudgements(_).map(relevanceToLabel)

    override def resultCount(a: A): Int =
      relevanceJudgements(a).length

    override def truePositiveCount(a: A): Int =
      relevanceJudgements(a).toList.count(identity)

    private def binaryLabels(a: A): NonEmptyList[Double] = relevanceJudgements(a).map(if(_) 1.0 else 0.0)
  }
  object BinaryRelevance {
    implicit val identityRelevanceJudgements: BinaryRelevance[NonEmptyList[Boolean]] = identity
    implicit def resultsWithRelInstance[A: Results : Qrels]: BinaryRelevance[A] = a => a.judgeWith(a.groundTruthSet.qrelSet)
    implicit val contravariantForRelevanceJudgements: Contravariant[BinaryRelevance] = new Contravariant[BinaryRelevance] {
      def contramap[A, B](fa: BinaryRelevance[A])(f: B => A): BinaryRelevance[B] = a => fa.relevanceJudgements(f(a))
    }
  }
}
