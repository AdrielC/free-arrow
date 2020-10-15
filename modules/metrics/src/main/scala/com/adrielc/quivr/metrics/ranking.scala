package com.adrielc.quivr.metrics

import cats.data.NonEmptyList
import simulacrum.{op, typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{Label, Rank}
import com.adrielc.quivr.metrics.function.Gain
import eu.timepit.refined.numeric._
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt

@typeclass trait LabelledSet[A] extends ResultCount[A] { self =>

  @op("labelledSet")
  def labelledSet(a: A): NonEmptyList[Double]

  @op("dcg")
  final def dcg(a: A): Option[Double] =
    calcDcg(labelledSet(a))

  @op("ndcg")
  final def ndcg(a: A, gain: Gain = Gain.Pow2): Option[Double] = {
    val res = labelledSet(a)
    val idealRank = res.sortBy(-_)
    for {
      dcg   <- calcDcg(res, gain)
      idcg  <- calcDcg(idealRank, gain)
      if idcg != 0
    } yield dcg / idcg
  }

  def asRelevanceJudgements(f: Label => Boolean): RelevanceJudgements[A] =
    labelledSet(_).map(f)

  override def resultCount(a: A): Int =
    labelledSet(a).length
}
object LabelledSet {
  implicit val labelledRanksInstance: LabelledSet[NonEmptyList[Label]] = identity
  implicit def labelledRanksFromResultsInstance[S: ResultSet: ResultLabels]: LabelledSet[S] = a => {
    val lab = a.resultLabels
    a.results.map(lab.lookup(_).getOrElse(0.0))
  }
  implicit def labelledRanksTupleRanksInstance[A: LabelledSet, B]: LabelledSet[(A, B)] = _._1.labelledSet
  implicit def labelledRanksRightTupleRanksInstance[A, B: LabelledSet]: LabelledSet[(A, B)] = _._2.labelledSet
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
  def precisionAtK(a: A, k: Rank): Option[Double] = {
    val rel = relevanceJudgements(a)
    (k <= PosInt.unsafeFrom(rel.size)).guard[Option].as(rel.toList.take(k).count(identity) / k.toDouble)
  }

  @op("recallAtK")
  def recallAtK(a: A, k: Rank): Option[Double] = {
    val rel = relevanceJudgements(a)
    val countRel = rel.count(identity)
    (k <= PosInt.unsafeFrom(rel.size) && countRel > 0).guard[Option].as(rel.toList.take(k).count(identity) / countRel.toDouble)
  }

  @op("fScoreAtK")
  def fScoreAtK(a: A, k: Rank): Option[Double] = {
    for {
      r <- recallAtK(a, k)
      p <- precisionAtK(a, k)
      plus = r + p
      if plus != 0
    } yield (2 * (r * p)) / plus
  }

  @op("rPrecision")
  def rPrecision(a: A): Option[Double] = {
    val rel = relevanceJudgements(a)
    val nRel = rel.count(identity)
    safeDiv(rel.toList.take(nRel.toInt).count(identity).toDouble, nRel.toDouble)
  }

  def asBinaryLabels: LabelledSet[A] =
    relevanceJudgements(_).map(if(_) 1.0 else 0.0)

  override def resultCount(a: A): Int =
    relevanceJudgements(a).length

  override def truePositiveCount(a: A): Int =
    relevanceJudgements(a).count(identity).toInt
}
object RelevanceJudgements {
  implicit val relevantRanksInstance: RelevanceJudgements[NonEmptyList[Boolean]] = identity
  implicit def resultsWithRelInstance[A: ResultSet : GroundTruthSet]: RelevanceJudgements[A] = GroundTruthRelevance[A]
  implicit def labelledRanksTupleRanksInstance[A: RelevanceJudgements, B]: RelevanceJudgements[(A, B)] = _._1.relevanceJudgements
  implicit def labelledRanksRightTupleRanksInstance[A, B: RelevanceJudgements]: RelevanceJudgements[(A, B)] = _._2.relevanceJudgements
}


@typeclass trait GroundTruthRelevance[A] extends RelevanceJudgements[A] with RelevantCounts[A] {
  implicit def R: ResultSet[A]; implicit def G: GroundTruthSet[A]
  override def relevanceJudgements(a: A): NonEmptyList[Boolean] = {
    val res = a.groundTruth
    a.results.map(res.contains)
  }
  override def groundTruthCount(a: A): Int =
    a.groundTruth.length
}
object GroundTruthRelevance {
  implicit def instance[A](implicit RS: ResultSet[A], GT: GroundTruthSet[A]): GroundTruthRelevance[A] =
    new GroundTruthRelevance[A] { val R: ResultSet[A] = RS; val G: GroundTruthSet[A] = GT }
}

