package com.adrielc.arrow

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._

import math.{log, pow}

object metrics {

  type Index = Int
  type Label = Double
  type ResultId = String

  case class LabelledResults(results: NonEmptyMap[Index, Label])
  case class RankingWithRelevants(ranking: NonEmptySet[ResultId], relevants: NonEmptySet[ResultId])
  case class Metric(key: String, value: Double)

  sealed trait EvalOp[A, B]

  object EvalOp {

    sealed trait RankingMetric[A] extends EvalOp[A, Metric]

    object Metric {

      case object Ndcg extends RankingMetric[LabelledResults] {

        def apply(results: LabelledResults): Double = {
          val dcg = calculateDCG(results)
          val idcg = calculateIDCG(results)

          if (idcg > 0)
            dcg / idcg
          else
            0.0
        }

        private def calculateDCG(labelledResults: LabelledResults): Double =
          labelledResults.results.toNel.foldMap { case (rel, i) => (pow(2, rel) - 1) / log2(i + 2) }

        private def calculateIDCG(labelledResults: LabelledResults): Double =
          calculateDCG(LabelledResults(labelledResults.results.toNel.sortBy(-_._2).toNem))

        private def log2(x: Double): Double = log(x) / log(2)
      }

      case object Recall extends RankingMetric[RankingWithRelevants] {

        def apply(results: RankingWithRelevants): Double =
          results.ranking.intersect(results.relevants).size / results.relevants.size
      }

      case object Precision extends RankingMetric[RankingWithRelevants] {

        def apply(results: RankingWithRelevants): Double =
          results.ranking.intersect(results.relevants).size / results.ranking.size
      }
    }
  }
}
