package com.adrielc.arrow
package free

import cats.data.AndThen
import com.adrielc.arrow.data.ArrowDescr
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
  case object RepeatN                 extends ConsoleArr[(String, Int), Unit]
  case class Prompt(message: String)  extends ConsoleArr[Unit, Unit]
  case class Const[A](value: A)       extends ConsoleArr[Unit, A]
  case class Dictionary(dict: Map[String, String]) extends ConsoleArr[String, Option[String]]

  implicit val functionInterpreter: ConsoleArr ~~> AndThen = new (ConsoleArr ~~> AndThen) {
    override def apply[A, B](f: ConsoleArr[A, B]): AndThen[A, B] = f match {
      case Prompt(message) => AndThen(_ => println(message))
      case GetLine => AndThen(_ => StdIn.readLine())
      case GetInt => AndThen(_ => Try(StdIn.readLong().toInt).getOrElse(1))
      case PutLine => AndThen(println)
      case RepeatN => AndThen((sn: (String, Int)) => for(_ <- 1 to sn._2) { println(sn._1) })
      case Const(value) => AndThen(_ => value)
      case Dictionary(dict) => AndThen(dict.get)
    }
  }

  val jsonInterpreter: ConsoleArr ~~> ArrowDescr = new (ConsoleArr ~~> ArrowDescr) {
    override def apply[A, B](f: ConsoleArr[A, B]): ArrowDescr[A, B] = f match {
      case Prompt(message) => ArrowDescr(Json.obj("Prompt" -> Json.obj("message" -> Json.fromString(message))))
      case Const(value) => ArrowDescr(Json.obj("Const" -> Json.obj("value" -> Json.fromString(value.toString))))
      case Dictionary(dict) => ArrowDescr(Json.obj("Dictionary" -> Json.fromFields(dict.mapValues(Json.fromString))))
      case command @ (GetLine | PutLine | RepeatN | GetInt) => ArrowDescr(command.toString)
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
    type FrCnsl[A, B] = F[ConsoleArr, A, B]
    val getLine: FrCnsl[Unit, String] = GetLine.lift[F]
    val getInt: FrCnsl[Unit, Int] = GetInt.lift[F]
    val putLine: FrCnsl[String, Unit] = PutLine.lift[F]
    val repeatN: FrCnsl[(String, Int), Unit] = RepeatN.lift[F]
    def prompt(message: String): FrCnsl[Unit, Unit] = Prompt(message).lift[F]
    def const[A](value: A): FrCnsl[Unit, A] = Const(value).lift[F]
    def dictionary(entry: (String, String)*): FrCnsl[String, Option[String]] = Dictionary(entry.toMap).lift[F]
  }
  object FreeConsole {
    @inline def apply[F[_[_, _], _, _] : ArrowF]: FreeConsole[F] = new FreeConsole[F]
  }
}