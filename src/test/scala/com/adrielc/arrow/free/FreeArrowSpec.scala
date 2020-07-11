package com.adrielc.arrow.free

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import ConsoleArr._
import com.adrielc.arrow.{MaybeFn, ~~>}
import syntax._

class FreeArrowSpec extends FlatSpec with Matchers {
  val freeArrow = FreeConsole[FreeArrow]
  import freeArrow._

  "ArrowDescr" should "render op Json" in {

    val program = GetLine.A >>> PutLine >>> GetInt >>> (_.toString) >>> PutLine

    val interpreter = stubGets andThen (functionInterpreter and jsonInterpreter)

    val f = program foldMap interpreter

    println(f._2.json.spaces2)

    f._1(())
  }


  def createTranslator(nComputations: Int): FrCnsl[Unit, Unit] = {

    val computeLoop = (compute >>> prompt("...")) loopN nComputations

    prompt("Hello") >>>
      prompt("Enter an English word to translate") >>>
      getLine -|> (
        ("Translating " + (_: String)) >>>
          putLine >>>
          computeLoop
        ) >>>
      dictionary (
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios"
      ) >>>
      (_.getOrElse("I don't know that one")) >>>
      putLine
  }


  "FreeArrow" should "run translator and count printlns" in {

    val translator = createTranslator(nComputations = 3)

    val countComputes = new (ConsoleArr ~~> λ[(α, β) => Int]) {
      def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
        case Compute => 1
        case _ => 0
      }
    }

    val withApology = new (ConsoleArr ~~> FrCnsl) {

      def apply[A, B](f: ConsoleArr[A, B]): FrCnsl[A, B] = f match {

        case d if d.isInstanceOf[Dictionary] => // isInstance needed here to stop type from being refined

          d.lift[FreeArrow].rmap(o => { println("sorry for the wait"); o })

        case _ =>

          f.lift[FreeArrow]
      }
    }

    val nComputes = translator analyze countComputes

    val optimized = if(nComputes >= 3) translator foldMap withApology else translator

    val runnable = optimized foldMap (stubGets andThen functionInterpreter)

    runnable(())
  }


  "FreeArrowPlus" should "add zero arrow" in {

    sealed trait Expr[A, B] extends Product with Serializable
    final case object Add extends Expr[(Int, Int), Int]
    final case object Sub extends Expr[(Int, Int), Int]

    import FAP._

    val z = zero[Int, Int]

    val plusZeroId: FreeArrowPlus[Expr, Int, Int] = z <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>> Add
    val subBoth = (plusZeroId &&& plusZeroId) >>> Sub
    val addAndSub = (addBoth &&& subBoth) >>> Sub

    val toMaybeOp = addAndSub.foldMap(new (Expr ~~> MaybeFn) {
      def apply[A, B](f: Expr[A, B]): MaybeFn[A, B] = f match {
        case Add => MaybeFn((ab: (Int, Int)) => ab._1 + ab._2)
        case Sub => MaybeFn((ab: (Int, Int)) => ab._1 - ab._2)
      }
    })

    assert(toMaybeOp(10) contains 20)
  }
}

