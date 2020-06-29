package com.adrielc.freearrow

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import ConsoleArr._

class FreeArrowSpec extends FlatSpec with Matchers {

  val translator = {
    import freeArrow._
    import FreeArrow.pure

    prompt("Hello") >>>
      prompt("Enter an English word to translate") >>>
      getLine -| (
        pure("Translating " + (_: String)) >>>
          putLine >>>
          (prompt("...").rmap(_ => Thread.sleep(1000))).loopN(3)
        ) >>>
      dictionary (
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios"
      ).rmap(_.getOrElse("I don't know that one")) >>>
      putLine
  }


  "ArrowDescr" should "render op Json" in {

    val program = {
      import freeArrowChoice._
      getLine >>> putLine >>> getInt.rmap(_.toString) >>> putLine
    }

    val interpreter = stubGets andThen (functionInterpreter and jsonInterpreter)

    val f = program foldMap interpreter

    println(f._2.json.spaces2)

    f._1(())
  }


  "FreeArrow" should "run translator and count printlns" in {

    val countPrintlns = new (ConsoleArr ~~> λ[(α, β) => Int]) {
      def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
        case PutLine | Prompt(_) => 1
        case _ => 0
      }
    }

    val nPrintlns = translator analyze countPrintlns

    val runnable = translator foldMap (stubGets andThen functionInterpreter)

    runnable(())

    assert(nPrintlns == 7)
  }
}
