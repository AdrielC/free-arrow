package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.instances.all._
import com.adrielc.arrow.exampleDsl.ConsoleArr
import ConsoleArr._
import com.adrielc.arrow.data.Tuple2A
import com.adrielc.arrow.{~>|}

class FreeArrowSpec extends FlatSpec with Matchers {
  import FreeA.{id, fn, lift, zeroArrow}


  "ArrowDescr" should "render op Json" in {

    val printInt = getInt >>^ (_.toString) >>> putLine

    val printLine = getLine >>> putLine

    val program3 = printLine >>> printInt

    val interpreter = stubGets.andThen(functionInterpreter and jsonInterpreter)

    val Tuple2A(f, json) = program3.foldMap(interpreter)

    println(json)

    f(())
  }


  "FreeArrowChoice" should "allow for choice" in {

    val program =
        (prompt("start left") +++ prompt("right")) >>>
        (const("done left") >>> putLine).left >>>
        (const("done right") >>> putLine).right

    val f = program.foldMap(functionInterpreter)

    f(<|)

    f(|>)
  }


  "FreeArrow" should "run translator and count printlns" in {

    val translator =
      prompt("Hello") >>>
        prompt("Enter an English word to translate") >>>
        getLine >| (
          fn("Translating " + (_: String)) >>>
            putLine >>>
            (compute >>> prompt("...")).loopN(3)
      ) >>>
      dictionary(
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios"
      ) >>^
      (_.getOrElse("I don't know that one")) >>>
      putLine

    val optimized = translator.optimize(
      new (ConsoleArr ~>| Int) {
        def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
          case Compute  => 1
          case _        => 0
        }
      },
      new (|~>[AC, ConsoleArr, Int]) {
        def apply[A, B](f: (Int, ConsoleArr[A, B])): FC[ConsoleArr, A, B] = f._2 match {

          case d if d.isInstanceOf[Dictionary]  =>

            val tested = lift(d) >>> fn((_: B) => f._1 > 3).test

            val finalize = id[B] ||| fn((o: B) => { println("sorry for the wait"); o })

            tested >>> finalize

          case other => lift(other)
        }
      }
    )

    val runnable = optimized.foldMap(stubGets andThen functionInterpreter)

    val idTest = id[String] ||| fn((o: String) => { println("sorry for the wait"); o })

    idTest.foldMap(functionInterpreter)


    runnable(())
  }


  "FreeArrowPlus" should "add zero arrow and mix in" in {

    import com.adrielc.arrow.exampleDsl.Expr._

    val plusZeroId = zeroArrow[Int, Int] <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>> add

    val add10 = num(10) >>> addBoth

    val choice = add10 ||| num(100)

    val comp2 = choice >| fn((n: Int) => println(n))

    val toMaybeOp = comp2.foldMap(toMaybeFn)

    assert(toMaybeOp(|>) contains 100)

    assert(toMaybeOp(<|) contains 20)
  }

  "FreeArrowChoicePlus" should "add" in {
    import com.adrielc.arrow.exampleDsl.Expr._

    val and = num(10) +++ (zeroArrow[Unit, Int] <+> num(20))

    val maybe = and.foldMap(toMaybeFn)

    val pure = and.foldMap(toPartialFn)

    assert(maybe(<|).contains(Left(10)))
    assert(pure(|>) == Right(20))
  }
}

