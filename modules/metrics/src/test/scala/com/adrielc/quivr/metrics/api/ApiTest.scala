package com.adrielc.quivr.metrics.api

import com.adrielc.quivr.metrics.MyEngagement, MyEngagement.{Click, Purchase}
import eu.timepit.refined.auto._
import org.scalatest.{FlatSpec, Matchers}

class ApiTest extends FlatSpec with Matchers {


  "Api" should "work" in {

    val eval = evaluator((
      count(Click: MyEngagement),
      count(Purchase: MyEngagement),
      count(Click: MyEngagement) ++ count(Purchase: MyEngagement),
      (count(Purchase: MyEngagement) / count(Click: MyEngagement)) | 0
    ))(
      f1,
      ndcg,
      avgPrecision,
    )(
      10,
      20,
      30,
      60
    )

    val results = eval(
      1L to 60,
      Map(
        2L -> Map((Click: MyEngagement) -> 1),
        53L -> Map((Click: MyEngagement) -> 1),
        40L -> Map((Purchase: MyEngagement) -> 2),
        46L -> Map((Purchase: MyEngagement) -> 6, (Click: MyEngagement) -> 1)
      )
    )

    assert(results == Map(
      "(click+purchase).f1.@60" -> 0.125,
      "(click+purchase).ndcg.@10" -> 0.6309297535714574,
      "(click+purchase).ap.@60" -> 0.17267227235438884,
      "purchase.f1.@60" -> 0.06451612903225806,
      "click.ap.@60" -> 0.20002734481815695,
      "(click+purchase).ndcg.@20" -> 0.6309297535714574,
      "purchase.ndcg.@60" -> 0.18340914219715643,
      "(click+purchase).f1.@20" -> 0.08333333333333334,
      "click.ndcg.@60" -> 0.4621111613131215,
      "click.ndcg.@20" -> 0.6309297535714574,
      "click.ap.@20" -> 0.5,
      "(click+purchase).ndcg.@60" -> 0.18662751500856462,
      "(click+purchase).ap.@30" -> 0.5,
      "click.f1.@60" -> 0.09523809523809523,
      "(click+purchase).f1.@10" -> 0.14285714285714288,
      "click.f1.@10" -> 0.15384615384615383,
      "click.ap.@10" -> 0.5,
      "click.ndcg.@30" -> 0.6309297535714574,
      "((purchase/click)|0).f1.@60" -> 0.03278688524590164,
      "(click+purchase).ndcg.@30" -> 0.6309297535714574,
      "purchase.ap.@60" -> 0.034239130434782605,
      "click.ap.@30" -> 0.5,
      "((purchase/click)|0).ndcg.@60" -> 0.18003132665669264,
      "(click+purchase).ap.@10" -> 0.5,
      "click.f1.@20" -> 0.08695652173913045,
      "((purchase/click)|0).ap.@60" -> 0.021739130434782608,
      "click.ndcg.@10" -> 0.6309297535714574,
      "click.f1.@30" -> 0.0606060606060606,
      "(click+purchase).f1.@30" -> 0.058823529411764705,
      "(click+purchase).ap.@20" -> 0.5
    ))
  }
}
