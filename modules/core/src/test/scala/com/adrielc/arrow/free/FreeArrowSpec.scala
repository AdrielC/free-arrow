package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.instances.all._
import com.adrielc.arrow.exampleDsl.{Cnsl, Expr}
import Cnsl.free._
import Expr.free._
import com.adrielc.arrow.data.EnvA
import com.adrielc.arrow.{BiFunctionK, ~>|}
import FreeA._
import cats.data.NonEmptyMap

import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

class FreeArrowSpec extends FlatSpec with Matchers {
  import FreeA.{id, fn, lift, zeroArrow}

  "ArrowDescr" should "render op Json" in {

    import Cnsl.~~>._

    val printInt = getInt >>^ (_.toString) >>>> putLine

    val printLine = getLine >>>> putLine

    val program3 = printLine >>>> printInt

    val interpreter = stubGets.andThen(function)

    val f = program3.foldMap(interpreter)

    f(())
  }


  "FreeArrowChoice" should "allow for choice" in {

    val program =
      (prompt("start left") +++ prompt("right")) >>>
        (const("done left") >>> putLine).left >>>
        (const("done right") >>> putLine).right

    val f = program.foldMap(Cnsl.~~>.function)

    f(^|)

    f(|^)
  }

  "BiFunctionK" should "use macro" in {

    val put = ("key", 1000)

    def mapHeadOption[A, B](map: NonEmptyMap[A, B]): (A, B) = (map: NonEmptyMap[A, B]).head

    val mapHead = BiFunctionK.lift[NonEmptyMap, Tuple2](mapHeadOption)

    val res = mapHead(NonEmptyMap.one(put._1, put._2))

    assert(res == put)
  }



  "FreeArrow" should "run translator, count printlns and optimize" in {
    import Cnsl.~~>._

    val translator =
      prompt("Hello") >>>
        prompt("Enter an English word to translate") >>>
        getLine ->| (
        fn("Translating " + (_: String)) >>>
          putLine >>>
          (compute >>> prompt("...")).loopN(4)
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
      new (Cnsl ~>| Int) {
        def apply[A, B](f: Cnsl[A, B]): Int = f match {
          case Cnsl.Compute   => 1
          case _              => 0
        }
      },
      new (|~>[Int, AC, Cnsl]) {
        def apply[A, B](f: EnvA[Int, Cnsl, A, B]): FC[Cnsl, A, B] = f._2 match {

          case d if d.isInstanceOf[Cnsl.Dictionary]  =>

            val tested = lift(d) >>> fn((_: B) => f._1.getConst.value > 3).test

            val finalize = id[B] ||| fn((o: B) => { println("sorry for the wait"); o })

            tested >>> finalize

          case other => lift(other)
        }
      }
    )

    val runnable = optimized.foldMap(stubGets andThen function)

    runnable(())
  }


  "FreeArrowPlus" should "add zero arrow and mix in" in {

    val plusZeroId = zeroArrow[Int, Int] <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>> add

    val add10 = num(10) >>> addBoth

    val choice = num(100) ||| add10

    val comp2 = choice ->| fn((n: Int) => println(n))

    val toMaybeOp = comp2.foldMap(Expr.~~>.function.kleisli[List])

    assert(toMaybeOp(|^) == List(20))

    assert(toMaybeOp(^|) == List(100))
  }

  "FreeA" should "infer R to be ArrowChoicePlus" in {

    val add10 = id[Int] ->/ num(10) >>> add

    val program = (add10 <+> zeroArrow <+> add10 <+> add10) >>> add10

    val run = program.foldMap(Expr.~~>.function.kleisli[List])

    assert(run(0) === List(20, 20, 20))
  }

  "FreeA.foldMap" should "not overflow" in {

    implicit val ec: ExecutionContext = ExecutionContext.global

    val add10Loop = (id[Int] ->/ num(1) >>> add).loopN(10000)

    val f = add10Loop.foldMap(Expr.~~>.function.kleisli[Future])

    println(Await.result(f(0), Duration(1000, MILLISECONDS)))
  }

  /**
   * [[cats.Id]] does not have a [[cats.MonoidK]] instance, so its kleisli can't
   * have a [[com.adrielc.arrow.ArrowChoiceZero]] instance
   *  */
  "(zeroArrow[Unit, Unit] ||| id[Unit]).foldMap(Expr.~~>.function.kleisli[cats.Id])" shouldNot compile

  "FreeA" should "combine algrabras" in {
    import Cnsl.~~>.stubGets

    val addN = num(10).second >>> add <<^ ((_: Int, ()))

    val getUserInt = prompt("Provide a number to add 10 to") >>> getInt

    val both = getUserInt.inl >>> addN.inr >>^ (_.toString) >>> putLine.inl

    stubGets.andThen(Cnsl.~~>.function).or(Expr.~~>.function)

    val run = both.foldMap(stubGets.andThen(Cnsl.~~>.function) or Expr.~~>.function)

    run(())
  }
}
