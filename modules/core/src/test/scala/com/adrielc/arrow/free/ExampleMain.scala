package com.adrielc.arrow.free

import cats.implicits._
import com.adrielc.arrow.data.{Pure, ~>|}

import scala.io.StdIn

object ExampleMain extends App {

  // DSL describing the core set of operations

  sealed trait ConsoleDSL[A, B]
  case object GetLine          extends ConsoleDSL[Unit, String]
  case object PutLine          extends ConsoleDSL[String, Unit]
  case class Prompt(s: String) extends ConsoleDSL[Unit, Unit]

  val pureConsole = new Pure[ConsoleDSL] {
    def apply[A, B](fab: ConsoleDSL[A, B]): A => B = fab match {
      case GetLine => _ => StdIn.readLine()
      case PutLine => println
      case Prompt(s) => _ => println(s)
    }
  }

  import FreeA._

  // Define smart constructors to lift dsl into FreeA
  val getLine = liftK(GetLine)
  val putLine = liftK(PutLine)
  def prompt(s: String) = liftK(Prompt(s))


  // construct program

  val program = prompt("Favorite album?") >>> getLine >>> lift("Blackwater Park is better than " + _) >>> putLine

  val or = getLine >>> putLine >>> zeroArrow[Unit, Unit]


  // interpret program

  val run = or.foldMap(pureConsole.kleisli[List])

  println("compiled")

  run(())
  // > Favorite album?
  // Thriller
  // > Blackwater Park is better than Thriller

  // Analyze program
  val description = or.analyze(new (ConsoleDSL ~>| String) {
    def apply[A, B](f: ConsoleDSL[A, B]): String = f match {
      case GetLine => "get "
      case PutLine => "put "
      case Prompt(s) => s"prompt($s) "
    }
  })

  println(description)
  // > prompt(Favorite album?) get put
}
