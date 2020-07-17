package com.adrielc.arrow

import com.adrielc.arrow.data.JsonA
import io.circe.Json

import scala.io.StdIn
import scala.util.Try

object exampleDsl {
  import com.adrielc.arrow.free.FreeArrow.lift

  sealed trait Expr[A, B] extends Product with Serializable
  object Expr {

    final case object Add extends Expr[(Int, Int), Int]
    final case object Sub extends Expr[(Int, Int), Int]
    final case class Num(n: Int) extends Expr[Unit, Int]

    val add = lift(Add)
    val sub = lift(Sub)
    def num(n: Int) = lift(Num(n))

    val toFn = new (Expr ~~> Function1) {
      def apply[A, B](f: Expr[A, B]): A => B = f match {
        case Add => ((ab: (Int, Int)) => ab._1 + ab._2)
        case Sub => ((ab: (Int, Int)) => ab._1 - ab._2)
        case Num(n) => (_ => n)
      }
    }

    val toMaybeFn = new (Expr ~~> MaybeFn) {
      def apply[A, B](f: Expr[A, B]): MaybeFn[A, B] = f match {
        case Add => MaybeFn((ab: (Int, Int)) => ab._1 + ab._2)
        case Sub => MaybeFn((ab: (Int, Int)) => ab._1 - ab._2)
        case Num(n) => MaybeFn(_ => n)
      }
    }
  }


  sealed trait ConsoleArr[-A, +B]
  object ConsoleArr {
    case object GetLine                 extends ConsoleArr[Unit, String]
    case object PutLine                 extends ConsoleArr[String, Unit]
    case object GetInt                  extends ConsoleArr[Unit, Int]
    case object Compute                 extends ConsoleArr[Unit, Unit]
    case object RepeatN                 extends ConsoleArr[(String, Int), Unit]
    case class Prompt(message: String)  extends ConsoleArr[Unit, Unit]
    case class Const[A](value: A)       extends ConsoleArr[Unit, A]
    case class Dictionary(dict: Map[String, String]) extends ConsoleArr[String, Option[String]]


    val getLine = lift(GetLine)
    val putLine = lift(PutLine)
    val getInt  = lift(GetInt)
    val compute = lift(Compute)
    val repeatN = lift(RepeatN)
    def prompt(message: String) = lift(Prompt(message))
    def const[A](value: A) = lift(Const(value))
    def dictionary(entries: (String, String)*) = lift(Dictionary(entries.toMap))

    implicit val functionInterpreter: ConsoleArr ~~> Function1 = new (ConsoleArr ~~> Function1) {
      override def apply[A, B](f: ConsoleArr[A, B]): A => B = f match {
        case Prompt(message) => _ => println(message)
        case GetLine => _ => StdIn.readLine()
        case GetInt => _ => Try(StdIn.readLong().toInt).getOrElse(1)
        case Compute => _ => Thread.sleep(1000) // trivial example
        case PutLine => println
        case RepeatN => (sn: (String, Int)) => for(_ <- 1 to sn._2) { println(sn._1) }
        case Const(value) => _ => value
        case Dictionary(dict) => dict.get
      }
    }

    val jsonInterpreter: ConsoleArr ~~> JsonA = new (ConsoleArr ~~> JsonA) {
      override def apply[A, B](f: ConsoleArr[A, B]): JsonA[A, B] = f match {
        case Prompt(message) => JsonA(Json.obj("Prompt" -> Json.obj("message" -> Json.fromString(message))))
        case Const(value) => JsonA(Json.obj("Const" -> Json.obj("value" -> Json.fromString(value.toString))))
        case Dictionary(dict) => JsonA(Json.obj("Dictionary" -> Json.fromFields(dict.mapValues(Json.fromString))))
        case command @ (GetLine | PutLine | RepeatN | GetInt | Compute) => JsonA(command.toString)
      }
    }

    val stubGets: ConsoleArr ~~> ConsoleArr = new (ConsoleArr ~~> ConsoleArr) {
      def apply[A, B](f: ConsoleArr[A, B]): ConsoleArr[A, B] = f match {
        case ConsoleArr.GetLine => Const("hello")
        case ConsoleArr.GetInt => Const(1)
        case other => other
      }
    }

    val countPrintlns: ConsoleArr ~>| Int = new (ConsoleArr ~>| Int) {
      def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
        case PutLine | Prompt(_)  => 1
        case _                    => 0
      }
    }
  }
}
