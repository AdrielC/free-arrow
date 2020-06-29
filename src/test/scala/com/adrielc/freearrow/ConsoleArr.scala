package com.adrielc.freearrow

import cats.data.AndThen
import io.circe.Json

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

  object freeArrow {
    val getLine: FrCnsl[Unit, String] = GetLine.ar
    val getInt: FrCnsl[Unit, Int] = GetInt.ar
    val putLine: FrCnsl[String, Unit] = PutLine.ar
    val repeatN: FrCnsl[(String, Int), Unit] = RepeatN.ar
    def prompt(message: String): FrCnsl[Unit, Unit] = Prompt(message).ar
    def const[A](value: A): FrCnsl[Unit, A] = Const(value).ar
    def dictionary(entry: (String, String)*): FrCnsl[String, Option[String]] = Dictionary(entry.toMap).ar
  }

  object freeArrowChoice {
    val getLine: FrCnslCh[Unit, String] = GetLine.ch
    val getInt: FrCnslCh[Unit, Int] = GetInt.ch
    val putLine: FrCnslCh[String, Unit] = PutLine.ch
    val repeatN: FrCnslCh[(String, Int), Unit] = RepeatN.ch
    def prompt(message: String): FrCnslCh[Unit, Unit] = Prompt(message).ch
    def const[A](value: A): FrCnslCh[Unit, A] = Const(value).ch
    def dictionary(entry: (String, String)*): FrCnslCh[String, Option[String]] = Dictionary(entry.toMap).ch
  }
}
