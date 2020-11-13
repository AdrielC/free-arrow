package com.adrielc.quivr.metrics.api

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.free.FA
import com.adrielc.quivr.metrics.{Rank, ResultRels, dsl}
import com.adrielc.quivr.metrics.dsl.+>
import com.adrielc.quivr.metrics.dsl.evaluation.{NoEngagements, NoResults}
import eu.timepit.refined.types.all.PosInt
import shapeless.Lazy
import cats.implicits._
import com.adrielc.quivr.metrics.data.EngagedResults

import scala.collection.immutable.SortedMap

/**
 *
 * @param labelers Descriptions of how to convert counts to relevancy labels
 * @param metrics Metrics you want to compute for each labeler
 * @param atK K values at which to compute each metric
 * @tparam L one or more of either Labeler[E] or Judge[E] e.g. (rel.count(Click), rel.binary(Purchase))
 * @return
 */
final case class Evaluator[L] (
  labelers: L,
  metrics : NonEmptyList[Metric],
  atK     : List[Rank]
) {

  def ++[M](other: Evaluator[M]): Evaluator[(L, M)] =
    Evaluator(
      labelers = (labelers, other.labelers),
      metrics = metrics ::: other.metrics,
      atK = atK ++ other.atK
    )

  def run(implicit L: Lazy[ToRel[L]]): EvalFunction[L.value.E] = {

    val in = FA.lift { (se: (Results, EngCounts[L.value.E])) =>
      (NonEmptyVector.fromVector(se._1.toVector).toRight(NoResults),
        NonEmptyMap.fromMap(SortedMap(se._2.toSeq: _*)).toRight(NoEngagements))
        .mapN((res, map) => EngagedResults(res, map))
    }

    val ks = atK.map(dsl.atK[ResultRels](_): ResultRels +> ResultRels).toNel.map(_.reduceK)

    val arrow = in >>> (L.value(labelers) >>> (ks getOrElse FA.id) >>> FA.plus(metrics)).right

    val f = arrow.run.rmap(_.map(_.flatten))

    (a, b) => f((a, b))
  }
}
object Evaluator {

  def apply[L]
  (labeler: L) // one or more of Labeler[E] or Judge[E] e.g. (rel.count(Click), rel.binary(Purchase))
  (metric: Metric, metrics: Metric*)
  (atKs: Int*)
  (implicit L: Lazy[ToRel[L]]): Evaluator[L] = {
    val _ = L
    Evaluator(
      labeler,
      NonEmptyList(metric, metrics.toList),
      atKs.distinct.toList.flatMap(PosInt.from(_).toOption)
    )
  }
}