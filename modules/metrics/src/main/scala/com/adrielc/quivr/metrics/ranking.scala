package com.adrielc.quivr.metrics

import cats.Contravariant
import cats.data.{NonEmptyList}
import com.adrielc.quivr.metrics.retrieval.{ResultCount, TruePositiveCount}
import com.adrielc.quivr.metrics.data.{Label, Rank}
import com.adrielc.quivr.metrics.function.Gain
import com.adrielc.quivr.metrics.result.{GroundTruthSet, ResultLabels, Results}
import simulacrum.{op, typeclass}
import cats.implicits._

object ranking {

  @typeclass trait RelevanceLabels[A] extends ResultCount[A] { self =>

    @op("relevanceLabels")
    def relevanceLabels(a: A): NonEmptyList[Double]

    @op("dcg")
    final def dcg(a: A): Double =
      calcDcg(relevanceLabels(a))

    @op("ndcg")
    final def ndcg(a: A, gain: Gain = Gain.Pow2): Option[Double] = {
      val res = relevanceLabels(a)
      val idealRank = res.sortBy(-_)
      val dcg = calcDcg(res, gain)
      val idcg = calcDcg(idealRank, gain)
      safeDiv(dcg, idcg)
    }

    def asRelevanceJudgements(f: Label => Boolean): RelevanceJudgements[A] =
      relevanceLabels(_).map(f)

    override def resultCount(a: A): Int =
      relevanceLabels(a).length
  }
  object RelevanceLabels {
    implicit val identityLabelledSet: RelevanceLabels[NonEmptyList[Label]] = identity
    implicit def labelledSetFromResultsInstance[S: Results: ResultLabels]: RelevanceLabels[S] = r => r.labelWith(r.resultLabels)
    implicit val contravariantForLabelledSet: Contravariant[RelevanceLabels] = new Contravariant[RelevanceLabels] {
      def contramap[A, B](fa: RelevanceLabels[A])(f: B => A): RelevanceLabels[B] = a => fa.relevanceLabels(f(a))
    }
  }

  @typeclass trait RelevanceJudgements[A] extends TruePositiveCount[A] { self =>

    @op("relevanceJudgements")
    def relevanceJudgements(a: A): NonEmptyList[Boolean]

    @op("averagePrecision")
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

    @op("reciprocalRank")
    def reciprocalRank(a: A): Option[Double] =
      relevanceJudgements(a).zipWithIndex
        .find { case (isRel, _) => isRel }
        .map { case (_, idx) => 1 / (idx + 1).toDouble }

    @op("precisionAtK")
    def precisionAtK(a: A, k: Rank): Option[Double] =
      relevanceJudgements(a).atK(k).flatMap(_.precision)

    @op("recallAtK")
    def recallAtK(a: A, k: Rank): Option[Double] = {
      val rel = relevanceJudgements(a)
      val countRel = rel.count(identity)
      relevanceJudgements(a).atK(k).flatMap { rel =>
        safeDiv(rel.toList.count(identity).toDouble, countRel.toDouble)
      }
    }

    @op("fScoreAtK")
    def fScoreAtK(a: A, k: Rank): Option[Double] =
      for {
        r <- recallAtK(a, k)
        p <- precisionAtK(a, k)
        plus = r + p
        if plus != 0
      } yield (2 * (r * p)) / plus

    @op("rPrecision")
    def rPrecision(a: A): Option[Double] = {
      val judgements = relevanceJudgements(a)
      val nRel = judgements.toList.count(identity)
      judgements.toList.take(nRel).toNel.flatMap(_.precision)
    }

    def asBinaryLabels(relevanceToLabel: Boolean => Label): RelevanceLabels[A] =
      relevanceJudgements(_).map(relevanceToLabel)

    override def resultCount(a: A): Int =
      relevanceJudgements(a).length

    override def truePositiveCount(a: A): Int =
      relevanceJudgements(a).toList.count(identity)
  }
  object RelevanceJudgements {
    implicit val identityRelevanceJudgements: RelevanceJudgements[NonEmptyList[Boolean]] = identity
    implicit def resultsWithRelInstance[A: Results : GroundTruthSet]: RelevanceJudgements[A] = a => a.judgeWith(a.groundTruthSet)
    implicit val contravariantForRelevanceJudgements: Contravariant[RelevanceJudgements] = new Contravariant[RelevanceJudgements] {
      def contramap[A, B](fa: RelevanceJudgements[A])(f: B => A): RelevanceJudgements[B] = a => fa.relevanceJudgements(f(a))
    }
  }
}
