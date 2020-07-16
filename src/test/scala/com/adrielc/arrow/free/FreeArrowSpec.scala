package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.instances.all._
import com.adrielc.arrow.exampleDsl.ConsoleArr
import ConsoleArr.{GetLine, Prompt, PutLine, _}
import cats.arrow.ArrowChoice
import com.adrielc.arrow.data.Tuple2A
import com.adrielc.arrow.{~>|}
import implicits._

class FreeArrowSpec extends FlatSpec with Matchers {
  import FreeA.{lift, id, fn}

  val printInt: FA[ConsoleArr, Unit, Unit] = ~GetInt >^ (_.toString) >>^ PutLine

  val printLine: FA[ConsoleArr, Unit, Unit] = ~GetLine >>^ PutLine

  "ArrowDescr" should "render op Json" in {

    val program3 = printLine >>> printInt

    val interpreter = stubGets.andThen(functionInterpreter and jsonInterpreter)

    val Tuple2A(f, json) = program3.foldMap(interpreter)

    println(json)

    f(())
  }


  "FreeArrowChoice" should "allow for choice" in {

    val program =
        (~Prompt("start left") +++ ~Prompt("right")) >>>
        (~Const("done left") >>^ PutLine).left >>>
        (~Const("done right") >>^ PutLine).right

    val f = program.foldMap(functionInterpreter)

    f(<|)

    f(|>)
  }


  "FreeArrow" should "run translator and count printlns" in {

    val translator = ~Prompt("Hello") >>^
      Prompt("Enter an English word to translate") >>^
      GetLine >| (
      ("Translating " + (_: String)) >>^
        PutLine >>>
        (~Compute >>^ Prompt("...")).loopN(3)
      ) >>^
      Dictionary(
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios"
      ) >^
      (_.getOrElse("I don't know that one")) >>^
      PutLine

    val optimized = translator.optimize[Int, ArrowChoice, ConsoleArr](
      new (ConsoleArr ~>| Int) {
        def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
          case Compute  => 1
          case _        => 0
        }
      },
      new (|~>[Int, ArrowChoice, ConsoleArr]) {
        def apply[A, B](f: (Int, ConsoleArr[A, B])): FAC[ConsoleArr, A, B] = f._2 match {

          case d if d.isInstanceOf[Dictionary]  =>

            val tested = lift(d) >>> fn((_: B) => f._1 > 3).test

            val finalize = (id[B] ||| fn((o: B) => { println("sorry for the wait"); o }))

            tested >>> finalize

          case other => lift(other)
        }
      }
    )

    val runnable = optimized.foldMap(stubGets andThen functionInterpreter)

    runnable(())
  }


  "FreeArrowPlus" should "add zero arrow and mix in" in {

    import com.adrielc.arrow.exampleDsl.Expr._

    import FreeA._

    val plusZeroId = zeroArrow[Int, Int] <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>^ Add

    val add10 = ~Num(10) >>> addBoth

    val choice = add10 ||| ~Num(100)

    val comp2 = choice >| ~((n: Int) => println(n))

    val toMaybeOp = comp2 foldMap toMaybeFn

    assert(toMaybeOp(|>) contains 100)

    assert(toMaybeOp(<|) contains 20)
  }

  "FreeArrowChoicePlus" should "add" in {
    import com.adrielc.arrow.exampleDsl.Expr._

    val and = ~Num(10) +++ ~Num(20)

    assert(and.foldMap(toMaybeFn).apply(<|).contains(Left(10)))
    assert((~Num(10)).foldMap(toFn).apply(()) == 10)
  }
}

