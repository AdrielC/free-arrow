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

  // requires at least one judged and one relevant result
  def apply(res: NonEmptyList[(ResultId, Relevance)], relCount: Int): Option[ResultRels] =
    (NonZeroCount.from(res.count(_._2.isJudged).toInt) *> NonZeroCount.from(relCount))
      .map(r => new ResultRels(res, Rank.unsafeFrom(res.size), r)).toOption

  implicit val resultRelsRelevanciesInstance: Relevancies.Aux[ResultRels, (ResultId, Relevance)] with RelevanceCounts[ResultRels] =
    new Relevancies[ResultRels] with RelevanceCounts[ResultRels] {
      override type Rel = (ResultId, Relevance)
      override val rel: Relevancy[(ResultId, Relevance)] = Relevancy.relevanceRelevancy.contramap(_._2)
      override def relevancies(a: ResultRels): NonEmptyList[(ResultId, Relevance)] = a.res.toList.take(a.k).toNel.get // safe because k is guaranteed >= 1
      override def groundTruthCount(a: ResultRels): Int = a.nRel
    }

  implicit val atK: AtK[ResultRels] = (a, k) => if(k > a.size) None else Some(ResultRels(a.res, k, a.nRel))
}


