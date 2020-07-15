package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.arrow.examples.ConsoleArr, ConsoleArr._
import com.adrielc.arrow.data.Tuple2A
import com.adrielc.arrow.{~>>, ~~>}
import implicits._

class FreeArrowSpec extends FlatSpec with Matchers {

  val printInt: FA[ConsoleArr, Unit, Unit] = ~GetInt >^ (_.toString) >>^ PutLine

  val printLine: FA[ConsoleArr, Unit, Unit] = ~GetLine >>^ PutLine

  "ArrowDescr" should "render op Json" in {

    val program3 = printLine >>> printInt

    val interpreter = stubGets andThen (functionInterpreter and jsonInterpreter)

    val Tuple2A(f, json) = program3 foldMap interpreter

    println(json)

    f(())
  }


  "FreeArrowChoice" should "allow for choice" in {

    val program =
      FreeA.fork >>>
        (~Prompt("start left") +++ ~Prompt("right")) >>>
        (~Const("done left") >>^ PutLine).left >>>
        (~Const("done right") >>^ PutLine).right

    val f = program foldMap functionInterpreter

    f(false)
  }


  def createTranslator(nComputations: Int): FA[ConsoleArr, Unit, Unit] = {

    ~Prompt("Hello") >>^
      Prompt("Enter an English word to translate") >>^
      GetLine >| (
        ("Translating " + (_: String)) >>^
          PutLine >>>
          (~Compute >>^ Prompt("...")).loopN(nComputations)
        ) >>^
      Dictionary(
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios"
      ) >^
      (_.getOrElse("I don't know that one")) >>^
      PutLine
  }


  "FreeArrow" should "run translator and count printlns" in {

    val translator = {

      val comp1 = createTranslator(nComputations = 1)

      val comp3 = createTranslator(nComputations = 3)

      ~((cmnd: String) => if(cmnd == "left") goL else goR ) >>> (comp1 ||| comp3)
    }

    val countComputes = new (ConsoleArr ~>> Int) {
      def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
        case ConsoleArr.Compute => 1
        case _ => 0
      }
    }

    val withApology = new (ConsoleArr ~~> FrCnsl) {

      def apply[A, B](f: ConsoleArr[A, B]): FrCnsl[A, B] = f match {

        case d if d.isInstanceOf[Dictionary] => ~d >^ (o => { println("sorry for the wait"); o })

        case _ => ~f
      }
    }

    val nComputes = translator analyze countComputes

    val optimized = if(nComputes >= 3) translator.foldMap(withApology) else translator

    val runnable = optimized foldMap (stubGets andThen functionInterpreter)

    runnable("right")
  }


  "FreeArrowPlus" should "add zero arrow and mix in" in {

    import com.adrielc.arrow.examples.Expr._

    import FreeA._

    val plusZeroId = zeroArrow[Int, Int] <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>^ Add

    val add10 = ~Num(10) >>> addBoth

    val choice = add10 ||| ~Num(100)

    val comp2 = choice >| ~((n: Int) => println(n))

    val toMaybeOp = comp2 foldMap toMaybeFn

    assert(toMaybeOp(goR) contains 100)

    assert(toMaybeOp(goL) contains 20)
  }

  "FreeArrowChoicePlus" should "add" in {
    import com.adrielc.arrow.examples.Expr._

    val and = ~Num(10) |&| ~Num(20)

    assert(and.foldMap(toMaybeFn).apply(()).contains(Left(10)))
    assert((~Num(10)).foldMap(toMaybeFn).apply(()).contains(10))
  }
}

