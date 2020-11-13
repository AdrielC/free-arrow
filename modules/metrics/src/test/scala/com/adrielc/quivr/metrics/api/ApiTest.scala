package com.adrielc.quivr.metrics.api

import cats.data.NonEmptyMap
import com.adrielc.quivr.metrics.api.metric._
import com.adrielc.quivr.metrics.api.rel._
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.quivr.metrics.dsl.evaluation.{NoValidJudgements, NoValidLabels}

class ApiTest extends FlatSpec with Matchers {

  "Api" should "work" in {

    sealed trait Eng1
    case object Click extends Eng1
    case object Purchase extends Eng1

    val eval1 = Evaluator((count(Click: Eng1), binary(Click: Eng1)))(ndcg, avgPrecision)(10, 20)


    sealed trait Eng2
    case object Reviews extends Eng2
    case object Rating extends Eng2

    val eval2 = Evaluator((count(Reviews: Eng2), binary(Rating: Eng2)))(ndcg, qMeasure, reciprocalRank)(10, 20, 30, 60)

    val e = (eval1 ++ eval2).run

    val results = e(
      1L to 60,
      Map(
        2L -> Map((Click: Eng1).asLeft[Eng2] -> 1),
        53L -> Map((Click: Eng1).asLeft[Eng2] -> 1),
        40L -> Map((Purchase: Eng1).asLeft[Eng2] -> 2),
        46L -> Map((Purchase: Eng1).asLeft[Eng2] -> 6)
      )
    )

    assert(results == NonEmptyMap.of(
      "binaryClick.ap.@10" -> Right(0.5),
      "binaryClick.ap.@20" -> Right(0.5),
      "binaryClick.ap.@30" -> Right(0.5),
      "binaryClick.ap.@60" -> Right(0.2688679245283019),
      "binaryClick.mrr.@10" -> Right(0.5),
      "binaryClick.mrr.@20" -> Right(0.5),
      "binaryClick.mrr.@30" -> Right(0.5),
      "binaryClick.mrr.@60" -> Right(0.5),
      "binaryClick.ndcg.@10" -> Right(0.6309297535714574),
      "binaryClick.ndcg.@20" -> Right(0.6309297535714574),
      "binaryClick.ndcg.@30" -> Right(0.6309297535714574),
      "binaryClick.ndcg.@60" -> Right(0.4933965394160924),
      "binaryClick.qMeasure.@10" -> Right(0.6666666666666666),
      "binaryClick.qMeasure.@20" -> Right(0.6666666666666666),
      "binaryClick.qMeasure.@30" -> Right(0.6666666666666666),
      "binaryClick.qMeasure.@60" -> Right(0.2863636363636364),
      "binaryRating" -> Left(NoValidJudgements),
      "click.ap.@10" -> Right(0.5),
      "click.ap.@20" -> Right(0.5),
      "click.ap.@30" -> Right(0.5),
      "click.ap.@60" -> Right(0.2688679245283019),
      "click.mrr.@10" -> Right(0.5),
      "click.mrr.@20" -> Right(0.5),
      "click.mrr.@30" -> Right(0.5),
      "click.mrr.@60" -> Right(0.5),
      "click.ndcg.@10" -> Right(0.6309297535714574),
      "click.ndcg.@20" -> Right(0.6309297535714574),
      "click.ndcg.@30" -> Right(0.6309297535714574),
      "click.ndcg.@60" -> Right(0.4933965394160924),
      "click.qMeasure.@10" -> Right(0.6666666666666666),
      "click.qMeasure.@20" -> Right(0.6666666666666666),
      "click.qMeasure.@30" -> Right(0.6666666666666666),
      "click.qMeasure.@60" -> Right(0.2863636363636364),
      "reviews" -> Left(NoValidLabels)
    ))
  }
}
