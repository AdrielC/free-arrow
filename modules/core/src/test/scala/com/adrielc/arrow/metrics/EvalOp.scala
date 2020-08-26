package com.adrielc.arrow.metrics

import scala.math.{log, pow, abs}
import cats.implicits._
import com.adrielc.arrow.metrics.EvalOp.EngagementOp.{BinaryEngagement, BinaryEngagementWeights, CountEngagementWeights, CountOf, LogDiscount, PercentOf}
import com.adrielc.arrow.metrics.EvalOp.EngagementType.{CartAdd, Click, Purchase}
import com.adrielc.arrow.metrics.evaluables.{LabelledIndexes, RankingWithRelevants}

sealed trait EvalOp[-A, +B]

object EvalOp {

  sealed trait Metric[-I] extends EvalOp[I, Double] with (I => Double)

  object Metric {

    case object Ndcg extends Metric[LabelledIndexes] {

      def apply(results: LabelledIndexes): Double = {
        val dcg = calculateDCG(results)
        val idcg = calculateIDCG(results)
        if (idcg > 0) dcg / idcg
        else 0.0
      }

      private def calculateDCG(labelledResults: LabelledIndexes): Double =
        labelledResults.results.toNel.foldMap { case (i, rel) => (pow(2, rel) - 1) / log2(i + 2.0) }

      private def calculateIDCG(labelledResults: LabelledIndexes): Double =
        calculateDCG(LabelledIndexes(labelledResults.results.toNel.sortBy(-_._2).zipWithIndex.map { case ((_, l), i) => i -> l } .toNem))

      private def log2(x: Double): Double = log(x) / log(2)
    }

    case object Recall extends Metric[RankingWithRelevants] {

      def apply(r: RankingWithRelevants): Double =
        r.results.intersect(r.relevant).size / r.relevant.size.toDouble
    }

    case object Precision extends Metric[RankingWithRelevants] {

      def apply(r: RankingWithRelevants): Double =
        r.results.intersect(r.relevant).size / r.results.size.toDouble
    }
  }


  case class AtK[A, B](k: Int, evalOp: EvalOp[A, B]) extends EvalOp[A, K[B]]

  case class K[A](k: Int, value: A) {
    override def toString: String = s"@$k"
  }

  sealed trait EngagementType extends Product with Serializable {
    def unary_! : EngagementOp = EngagementOp.BinaryEngagement(this)
    def unary_+ : EngagementOp = EngagementOp.CountOf(this)
    def /(other: EngagementType): EngagementOp = EngagementOp.PercentOf(this, other)
  }
  object EngagementType {
    case object Click     extends EngagementType
    case object CartAdd   extends EngagementType
    case object Purchase  extends EngagementType
  }


  sealed trait EngagementOp extends EvalOp[EngagementCounts, Double] {

    def apply(engs: EngagementCounts): Double = this match {
      case BinaryEngagement(e)                => engs.get(e).fold(0.0)(_.binarize.toDouble)
      case BinaryEngagementWeights(weights)   => engs.counts.map { case (e, n) => weights.getOrElse(e, 0.0) * n }.max
      case CountOf(e)                         => engs.counts.getOrElse(e, 0).toDouble
      case CountEngagementWeights(weights)    => engs.counts.map { case (e, n) => weights.getOrElse(e, 0.0) * n }.sum
      case PercentOf(e, by)                   => val per = engs.counts.getOrElse(by, 0); if(per <= 0) 0.0 else engs.counts.getOrElse(e, 0) / per.toDouble
      case LogDiscount(l, eng)                => log(abs(eng(engs)) + 1) / log(l)
    }
  }

  object EngagementOp {

    case class BinaryEngagement(e: EngagementType)                    extends EngagementOp
    case class BinaryEngagementWeights(w: Map[EngagementType, Double])  extends EngagementOp
    case class CountOf(e: EngagementType)                               extends EngagementOp
    case class CountEngagementWeights(w: Map[EngagementType, Double])   extends EngagementOp
    case class PercentOf(e: EngagementType, by: EngagementType)         extends EngagementOp
    case class LogDiscount(l: Int, e: EngagementOp)                     extends EngagementOp
  }

  case class EngagementCounts(counts: Map[EngagementType, Long]) {
    def get(e: EngagementType): Option[Long] = counts.get(e)
    def apply(e: EngagementType): Long = counts.getOrElse(e, 0)
    lazy val count    : Long = counts.toList.foldMap(_._2)
    lazy val clicks   : Long = counts.getOrElse(Click, 0)
    lazy val cartAdds : Long = counts.getOrElse(CartAdd, 0)
    lazy val purchases: Long = counts.getOrElse(Purchase, 0)
  }
}