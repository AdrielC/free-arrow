package com.adrielc.quivr.metrics.dsl

import cats.data.{NonEmptyMap, NonEmptyVector}
import com.adrielc.quivr.free.FreeArrow
import com.adrielc.quivr.metrics.{MyEngagement}
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.auto._
import com.adrielc.quivr.metrics.data.EngagedResults
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp

import com.adrielc.quivr.~~>

class MetricBuilderSpec extends FlatSpec with Matchers {
  import MyEngagement._

  type Res = EngagedResults[MyEngagement]

  val results = EngagedResults(
    NonEmptyVector.fromVectorUnsafe((1L to 200L).toVector),
    NonEmptyMap.of(
      1L -> (1.click + 1.cartAdd + 2.purchases),
      20L -> (2.clicks + 2.cartAdds + 2.purchases),
      30L -> (3.clicks + 3.cartAdds + 3.purchases),
      40L -> (4.clicks + 4.cartAdds + 4.purchases),
      60L -> (7.clicks + 7.cartAdds + 7.purchases)
    )
  )

  "combine" should "build" in {

    val labeler = label.from[Res](purchases, clicks && (purchases.filter === 7))

    val metrics = labeler >>> atK(50) >>&& (eval.recall, eval.precision)

    val res = metrics.run(results)

    val purchaseKey = "purchase.(recall,prec).@50"

    print(res)

    assert(res.length == 2)
    assert(res.lookup(purchaseKey).exists(_.contains((0.8, 0.08))))
    assert(res.-(purchaseKey).headOption.exists(_._2.isLeft))
  }

  "ranking metrics" should "be applied" in {

    val evaluator =
      (^[Res] >>> atK(10, 20) >>> label(clicks, cartAdds, purchases) >>> eval.ndcg) <+>
        (^[Res] >>> atK(30, 40) >>> judge(anyClicks) >>> eval.averagePrecision)

    val res = evaluator.run(results)

    assert(res.toSortedMap == Map(
      "binaryClick.ap.@30" -> Right(0.4000000000000001),
      "binaryClick.ap.@40" -> Right(0.32500000000000007),
      "cartadd.ndcg.@10"   -> Right(1.0),
      "cartadd.ndcg.@20"   -> Right(0.46352060224668756),
      "click.ndcg.@10"     -> Right(1.0),
      "click.ndcg.@20"     -> Right(0.46352060224668756),
      "purchase.ndcg.@10"  -> Right(1.0),
      "purchase.ndcg.@20"  -> Right(0.7527425666302089)
    ))
    assert(res.lookup("binaryClick.ap.@40").isDefined)
    assert(res.lookup("binaryClick.ap.@40").exists(_.contains(0.32500000000000007)))
  }

  "EvalOp" should "annotate" in {

    val evaluation2 = clicks.from[ResultEngs] >>> atK(60) >>> eval.qMeasure(1)

    type Annotated[A, B] = A >> (List[EvalOp[_, _]], B)

    val res = evaluation2.foldMap[Annotated](new (EvalOp ~~> Annotated) {
      override def apply[A, B](fab: EvalOp[A, B]): Annotated[A, B] =
        FreeArrow.liftK(fab).rmap(o => (List(fab), o))
    }).rmap(a => interpreter.key.defaultKeyBuilder.summarize(a._1) -> a._2).run(results)

    println(res)

    assert(res._2.contains(("click.q.@60",0.25056636689763623)))
  }
}
