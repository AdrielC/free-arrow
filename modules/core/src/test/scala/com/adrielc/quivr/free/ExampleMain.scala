package com.adrielc.quivr
package free

import cats.implicits._
//import com.adrielc.quivr.instances.all._

import scala.io.StdIn

object ExampleMain extends App {

  // DSL describing the core set of operations

  sealed trait ConsoleDSL[A, B]
  case object GetLine           extends ConsoleDSL[Unit, String]
  case object GetInt            extends ConsoleDSL[Unit, Int]
  case object PutLine           extends ConsoleDSL[String, Unit]
  case class AddInt(i: Int)     extends ConsoleDSL[Int, Int]
  case class Prompt(s: String)  extends ConsoleDSL[Unit, Unit]

  val pureConsole = new Pure[ConsoleDSL] {
    def toFn[A, B](fab: ConsoleDSL[A, B]): A => B = fab match {
      case GetLine => _ => StdIn.readLine()
      case PutLine => println
      case Prompt(s) => _ => println(s)
      case GetInt => _ => scala.util.Try(StdIn.readLine().toInt).getOrElse(0)
      case AddInt(i) => a => a + i
    }
  }

  import FreeArrow._

  // Define smart constructors to lift dsl into FreeA
  val getLine = liftK(GetLine)
  val putLine = liftK(PutLine)
  val getInt = liftK(GetInt)
  def addInt(n: Int) = liftK(AddInt(n))
  def prompt(s: String) = liftK(Prompt(s))


  // construct program

  val program =
    prompt("Favorite album?") >>>
      getLine >^
      ("Blackwater Park is better than " + _) >>>
      putLine >>>
      getInt >^
      (_.toString) >>>
      putLine

  val or = getLine >>> putLine >>> zeroArrow

  // interpret program

  val run = (
    prompt("give int") >>>
      getInt >>>
      addInt(1).loopN(10000000) >^
      (_.toString) >>>
      putLine
    ).foldMap(pureConsole)

  println("compiled")

  run(())
  // > Favorite album?
  // Thriller
  // > Blackwater Park is better than Thriller

  // Analyze program
  val description = program.analyze(new (ConsoleDSL ~>| String) {
    def apply[A, B](f: ConsoleDSL[A, B]): String = f match {
      case GetLine => "get line\n"
      case GetInt => "get int\n"
      case PutLine => "put\n"
      case Prompt(s) => s"prompt '$s'\n"
      case AddInt(i) => s"add $i\n"
    }
  })

  println(description)
  // > prompt(Favorite album?) get put
}
