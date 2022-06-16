package com.adrielc.quivr.metrics
package api

import com.adrielc.quivr.metrics.MyEngagement.{Click, Purchase}
import eu.timepit.refined.auto._
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import scalaz.syntax.all._

import scala.collection.immutable.SortedMap

class ApiTest extends FlatSpec with Matchers {

  "Api" should "work" in {

    val eval = evaluator(
      (
        count(Click: MyEngagement),
        count("Hello"),
      )
    )(
      f1, ndcg, avgPrecision,
    )(
      2, 10, 20, 30
    )

    val results = eval(
      1L to 100L,
      Map(
        1L -> Map(Left(Click: MyEngagement) -> 1),
        2L -> Map(Left(Click: MyEngagement) -> 1),
        40L -> Map(Left(Purchase: MyEngagement) -> 2),
        5L -> Map(Right("Hello") -> 6)
      )
    )

    assert(SortedMap(results.toList:_*) == SortedMap(
      "click.ap.@2" -> 1.0,
      "click.f1.@2" -> 0.6666666666666666,
      "click.ndcg.@2" -> 1.0
    ))
  }
}
