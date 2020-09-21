package com.adrielc.quivr.metrics

import com.adrielc.quivr.metrics.EngagementType.Purchase
import com.adrielc.quivr.metrics.EvalOp.free._
import com.adrielc.quivr.metrics.LabelOp.free._
import com.adrielc.quivr.metrics.evaluable.{LabelledIndexes, Results}
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.quivr.data.~>|
import com.adrielc.quivr.free.FreeArrow

class FreeEvalTest extends FlatSpec with Matchers {

  val labelledIndexes = LabelledIndexes.of(
    1 -> 3,
    2 -> 2,
    3 -> 3,
    4 -> 0,
    5 -> 1,
    6 -> 2
  )

  val labelledIndexes2 = LabelledIndexes.of(
    5 -> 1.0,
    1 -> 7.0,
    6 -> 3.0,
    2 -> 3.0,
    3 -> 7.0
  )

  val results = Results.of(
    1L -> 3.purchases.some,
    2L -> 2.purchases.some,
    3L -> 3.purchases.some,
    4L -> none,
    5L -> 1.purchases.some,
    6L -> 2.purchases.some
  )

  "Free Eval" should "evaluate correctly" in {

    val eval =
      ((withLabeler(countOf(Purchase)) >>^
        (LabelledIndexes(_)) right
        (atK[LabelledIndexes](6) right ndcg))
          .summarizeWith(new (EvalOp ~>| List[String]) {
            def apply[A, B](fab: EvalOp[A, B]): List[String] = fab match {
              case EvalOp.WithLabeler(l)  => l.analyze(collectOps[LabelOp]).map(_.toString)
              case other                  => List(other.toString)
            }
          })) >>> FreeArrow.lift((_: List[String]).mkString(".")).first

    val res = eval.apply(results)

    assertResult(("CountOf(Purchase).AtK(6).Ndcg", Right(Right(0.9488107485678984))))(res)
  }
}

