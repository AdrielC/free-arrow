package com.adrielc.arrow.free

import cats.implicits._
import com.adrielc.arrow.{~~>, ~>|}

import scala.io.StdIn

object ExampleMain extends App {

  // DSL describing the core set of operations

  sealed trait ConsoleDSL[A, B]
  case object GetLine          extends ConsoleDSL[Unit, String]
  case object PutLine          extends ConsoleDSL[String, Unit]
  case class Prompt(s: String) extends ConsoleDSL[Unit, Unit]

  import FreeA.{lift, fn}

  // Define smart constructors to lift dsl into FreeA
  val getLine = lift(GetLine)
  val putLine = lift(PutLine)
  def prompt(s: String) = lift(Prompt(s))


  // construct program

  val program = prompt("Favorite album?") >>> getLine >>> fn("Blackwater Park is better than " + _) >>> putLine


  // interpret program

  val run = program.foldMap(new (ConsoleDSL ~~> Function1) {
    override def apply[A, B](f: ConsoleDSL[A, B]): A => B = f match {
      case GetLine => _ => StdIn.readLine()
      case PutLine => println
      case Prompt(s) => _ => println(s)
    }
  })

  run(())
  // > Favorite album?
  // Thriller
  // > Blackwater Park is better than Thriller

  // Analyze program
  val description = program.analyze(new (ConsoleDSL ~>| String) {
    def apply[A, B](f: ConsoleDSL[A, B]): String = f match {
      case GetLine => "get "
      case PutLine => "put "
      case Prompt(s) => s"prompt($s) "
    }
  })

  println(description)
  // > prompt(Favorite album?) get put
}
