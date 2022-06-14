package com.adrielc.quivr
package free

import org.scalatest.{FlatSpec, Matchers}
import cats.instances.all._
import instances.all._
import com.adrielc.quivr.data.exampleDsl.Cnsl.free._
import com.adrielc.quivr.data.exampleDsl.Expr.free._
import com.adrielc.quivr.data.EnvA
import cats.data.NonEmptyMap
import com.adrielc.quivr.data.exampleDsl.{Cnsl, Expr}

import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

class FreeArrowSpec extends FlatSpec with Matchers {
  import FreeArrow._

  "FreeArrow" should "not stack overflow" in {

    val add1 = FreeArrow.lift((_: Int) + 1)

    val run = List.fill(100000)(add1).reduce(_ andThen _).fold[Function1].apply(0)

    assert(run == 100000)
  }

  "ArrowDescr" should "render op Json" in {

    import com.adrielc.quivr.data.exampleDsl.Cnsl.~~>._

    val printInt = getInt >^ (_.toString) >>> putLine

    val printLine = getLine >>> putLine

    val program3 = printLine >>> printInt

    val f = program3.foldMap(stubGets.andThen(function))

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



  val translator =
    prompt("Hello") >>>
      prompt("Enter an English word to translate") >>>
      getLine >>| ( // dead end, return output of `getLine` after the following
        putLine.lmap("Translating " + (_: String)) >>>
        prompt("...").rmap(_ => Thread.sleep(1000)).loopN(3)) >>>
      dictionary(
        "apple" -> "manzana",
        "blue" -> "azul",
        "hello" -> "hola",
        "goodbye" -> "adios")
        .rmap(_.getOrElse("I don't know that one")) >>>
      putLine


  "FreeArrow" should "run translator, count printlns and optimize" in {
    import Cnsl.~~>._

    val optimized = translator.optimize(
      new (Cnsl ~>| Int) {
        def apply[A, B](f: Cnsl[A, B]): Int = f match {
          case Cnsl.Compute   => 1
          case _              => 0
        }
      },
      new (|~>[Int, AC, Cnsl]) {
        def apply[A, B](f: EnvA[Int, Cnsl, A, B]): FAC[Cnsl, A, B] = f._2 match {

          case d if d.isInstanceOf[Cnsl.Dictionary]  =>

            liftK(d) >>> lift((_: B) => f._1.getConst.value > 3).test >^ (_.fold(
              identity,
              (o: B) => { println("sorry for the wait"); o }
            ))

          case other => liftK(other)
        }
      }
    )

    val runnable = optimized.foldMap(stubGets.andThen(function))

    runnable(())
  }

  "FreeA" should "run translator not evaluate analyzer if optimizer doesn't use value" in {
    import Cnsl.~~>._

    var folds = 0

    val optimized = translator.optimize(
      new (Cnsl ~>| Int) {
        def apply[A, B](f: Cnsl[A, B]): Int = {

          folds += 1 // This shouldn't execute ever if the sum value is not referenced below

          f match {
            case Cnsl.Compute   => 1
            case _              => 0
          }
        }
      },
      new (|~>[Int, AC, Cnsl]) {
        def apply[A, B](f: EnvA[Int, Cnsl, A, B]): FAC[Cnsl, A, B] = FA.liftK(f._2)
      }
    )

    val runnable = optimized.foldMap(stubGets.andThen(function))

    runnable(())

    assert(folds == 0)
  }


  "FreeArrowPlus" should "add zero arrow and mix in" in {

    val plusZeroId = zeroArrow[Int, Int] <+> id

    val addBoth = (plusZeroId &&& plusZeroId) >>> add

    val add10 = num(10) >>> addBoth

    val choice = num(100) ||| add10

    val comp2 = choice >>| lift((n: Int) => println(n))

    val toMaybeOp = comp2.foldMap(Expr.~~>.function.kleisli[List])

    assert(toMaybeOp(|^) == List(20))

    assert(toMaybeOp(^|) == List(100))
  }

  "FreeA" should "infer R to be ArrowChoicePlus" in {

    val add10 = id[Int] >>/ num(10) >>> add

    val program = (add10 <+> zeroArrow <+> add10 <+> add10) >>> add10

    val run = program.foldMap(Expr.~~>.function.kleisli[List])

    assert(run(0) === List(20, 20, 20))
  }

  "FreeA.foldMap" should "not overflow" in {

    implicit val ec: ExecutionContext = ExecutionContext.global

    val add10Loop = (id[Int] >>/ num(1) >>> add).loopN(10000)

    val f = add10Loop.foldMap(Expr.~~>.function.kleisli[Future])

    println(Await.result(f(0), Duration(1000, MILLISECONDS)))
  }

  /**
   * [[cats.Id]] does not have a [[cats.MonoidK]] instance, so its kleisli can't
   * have a [[com.adrielc.quivr.ArrowChoiceZero]] instance
   *  */
  "(zeroArrow[Unit, Unit] ||| id[Unit]).foldMap(Expr.~~>.function.kleisli[cats.Id])" shouldNot compile

  "FreeA" should "combine algrabras" in {
    import Cnsl.~~>.stubGets

    val a = num(10).second >>> add

    val addN = a <^ ((_: Int, ()))

    val getUserInt = prompt("Provide a number to add 10 to") >>> getInt

    val both = getUserInt.inl[Expr] >>>^ addN >^ (_.toString) >>>^ putLine

    stubGets.andThen(Cnsl.~~>.function).or(Expr.~~>.function)

    val run = both.foldMap(stubGets.andThen(Cnsl.~~>.function) or Expr.~~>.function)

    run(())
  }

  "FreeA" should "infer capabilities" in {

    val unit: FreeArrow[Arrow,      Nothing, Unit, Unit]                = FreeArrow.id[Unit]

    val ar: FreeArrow[Arrow,        Nothing, Unit, Unit]                = unit >>> unit

    val _: FreeArrow[ArrowChoice,   Nothing, Either[Unit, Unit], Unit]  = ar ||| ar

    val ap: FreeArrow[ArrowPlus,    Nothing, Unit, Unit]                = ar <+> ar <+> ar

    val az: FreeArrow[ArrowZero,    Nothing, Unit, Unit]                = ap >>> zeroArrow[Unit, Unit] <+> ar <+> ar

    val run = az.foldMap(BiFunctionK.id[Function1].kleisli[List])

    assert(run(()) === List((), ()))
  }
}

