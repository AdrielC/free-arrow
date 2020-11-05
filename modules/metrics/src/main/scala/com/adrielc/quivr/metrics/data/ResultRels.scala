package com.adrielc.quivr.metrics.data

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.data.relevance.Relevance
import com.adrielc.quivr.metrics.result.AtK
import cats.implicits._
import com.adrielc.quivr.metrics.ranking.Relevancies
import com.adrielc.quivr.metrics.relevancy.Relevancy
import com.adrielc.quivr.metrics.retrieval.RelevanceCounts
import eu.timepit.refined.auto._

case class ResultRels private[metrics] (res: NonEmptyList[(ResultId, Relevance)], k: Rank, nRel: NonZeroCount) {
  lazy val size: NonZeroCount = NonZeroCount.unsafeFrom(res.size)
}
object ResultRels {

  // guarantees that if a list of relevancies is returned then there is at least one judged result
  def apply[V](res: NonEmptyList[ResultId], labels: Map[ResultId, V], judgeLabel: V => Relevance): Option[ResultRels] = {
    val rels = labels.mapValues(judgeLabel)
    val results = res.map(id => id -> rels.getOrElse(id, Relevance.unjudged))
    (NonZeroCount.from(results.count(_._2.isJudged).toInt) *> NonZeroCount.from(rels.count(_._2.isRel)))
      .map(r => new ResultRels(results, Rank.unsafeFrom(results.size), r)).toOption
  }

  implicit val resultRelsRelevanciesInstance: Relevancies.Aux[ResultRels, (ResultId, Relevance)] with RelevanceCounts[ResultRels] =
    new Relevancies[ResultRels] with RelevanceCounts[ResultRels] {
      override type Rel = (ResultId, Relevance)
      override val rel: Relevancy[(ResultId, Relevance)] = Relevancy.relevanceRelevancy.contramap(_._2)
      override def relevancies(a: ResultRels): NonEmptyList[(ResultId, Relevance)] = a.res.toList.take(a.k).toNel.get // safe because k is guaranteed >= 1
      override def groundTruthCount(a: ResultRels): Int = a.nRel
    }

  implicit val atK: AtK[ResultRels] = (a, k) => if(k > a.size) None else Some(ResultRels(a.res, k, a.nRel))
}


