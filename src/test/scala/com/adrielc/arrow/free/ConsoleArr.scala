package com.adrielc.arrow
package free

import com.adrielc.arrow.data.ArrowDescr
import com.adrielc.arrow.free.methods.ArrowF
import io.circe.Json
import syntax._

import scala.io.StdIn
import scala.util.Try

sealed trait ConsoleArr[-A, +B]
object ConsoleArr {
  type FrCnsl[A, B] = FreeArrow[ConsoleArr, A, B]
  type FrCnslCh[A, B] = FreeArrowChoice[ConsoleArr, A, B]

  case object GetLine                 extends ConsoleArr[Unit, String]
  case object GetInt                  extends ConsoleArr[Unit, Int]
  case object PutLine                 extends ConsoleArr[String, Unit]
  case object Compute                 extends ConsoleArr[Unit, Unit]
  case object RepeatN                 extends ConsoleArr[(String, Int), Unit]
  case class Prompt(message: String)  extends ConsoleArr[Unit, Unit]
  case class Const[A](value: A)       extends ConsoleArr[Unit, A]
  case class Dictionary(dict: Map[String, String]) extends ConsoleArr[String, Option[String]]

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

  val jsonInterpreter: ConsoleArr ~~> ArrowDescr = new (ConsoleArr ~~> ArrowDescr) {
    override def apply[A, B](f: ConsoleArr[A, B]): ArrowDescr[A, B] = f match {
      case Prompt(message) => ArrowDescr(Json.obj("Prompt" -> Json.obj("message" -> Json.fromString(message))))
      case Const(value) => ArrowDescr(Json.obj("Const" -> Json.obj("value" -> Json.fromString(value.toString))))
      case Dictionary(dict) => ArrowDescr(Json.obj("Dictionary" -> Json.fromFields(dict.mapValues(Json.fromString))))
      case command @ (GetLine | PutLine | RepeatN | GetInt | Compute) => ArrowDescr(command.toString)
    }
  }

  val stubGets: ConsoleArr ~~> ConsoleArr = new (ConsoleArr ~~> ConsoleArr) {
    def apply[A, B](f: ConsoleArr[A, B]): ConsoleArr[A, B] = f match {
      case ConsoleArr.GetLine => Const("hello")
      case ConsoleArr.GetInt => Const(1)
      case other => other
    }
  }

  val countPrintlns: ConsoleArr ~~> λ[(α, β) => Int] = new (ConsoleArr ~~> λ[(α, β) => Int]) {
    def apply[A, B](f: ConsoleArr[A, B]): Int = f match {
      case PutLine | Prompt(_) => 1
      case _ => 0
    }
  }

  class FreeConsole[F[_[_, _], _, _] : ArrowF] {
    val getLine = GetLine.lift[F]
    val getInt = GetInt.lift[F]
    val putLine = PutLine.lift[F]
    val repeatN = RepeatN.lift[F]
    val compute = Compute.lift[F]
    def prompt(message: String) = Prompt(message).lift[F]
    def const[A](value: A) = Const(value).lift[F]
    def dictionary(entry: (String, String)*) = Dictionary(entry.toMap).lift[F]
  }
  object FreeConsole {
    @inline def apply[F[_[_, _], _, _] : ArrowF]: FreeConsole[F] = new FreeConsole[F]
  }
}
