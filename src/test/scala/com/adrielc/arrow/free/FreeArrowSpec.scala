package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import ConsoleArr._
import syntax._

class FreeArrowSpec extends FlatSpec with Matchers {

  "ArrowDescr" should "render op Json" in {

    val program = GetLine.ar >>> PutLine >>> GetInt >>^ (_.toString) >>> PutLine

    val interpreter = stubGets andThen (functionInterpreter and jsonInterpreter)

    val f = program foldMap interpreter

    println(f._2.json.spaces2)

    f._1(())
  }


  "FreeArrow" should "run translator and count printlns" in {

    val translator = {
      val freeArrow = FreeConsole[FreeArrow]
      import freeArrow._

      prompt("Hello") >>>
        prompt("Enter an English word to translate") >>>
        getLine -| (
          ("Translating " + (_: String)) ^>>
            putLine >>>
              (prompt("...") >>^ (_ => Thread.sleep(1000))).loopN(3)
          ) >>>
        dictionary (
          "apple" -> "manzana",
          "blue" -> "azul",
          "hello" -> "hola",
          "goodbye" -> "adios"
        ).rmap(_.getOrElse("I don't know that one")) >>>
        putLine
    }

    val nPrintlns = translator analyze countPrintlns

    val runnable = translator foldMap (stubGets andThen functionInterpreter)

    runnable(())

    assert(nPrintlns == 7)
  }
}
