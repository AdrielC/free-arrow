package com.adrielc.arrow

import cats.data.AndThen

import scala.io.StdIn
import scala.util.Try

object exampleDsl {
  import com.adrielc.arrow.free.FreeA.lift

  sealed trait Expr[A, B] extends Product with Serializable
  object Expr {

    final case object Add extends Expr[(Int, Int), Int]
    final case object Sub extends Expr[(Int, Int), Int]
    final case class Num(n: Int) extends Expr[Unit, Int]

    object free {
      val add = lift(Add)
      val sub = lift(Sub)
      def num(n: Int) = lift(Num(n))
    }

    object ~~> {

      val function = new (Expr ~~> Function1) {
        def apply[A, B](f: Expr[A, B]): A => B = f match {
          case Add => AndThen((ab: (Int, Int)) => ab._1 + ab._2)
          case Sub => AndThen((ab: (Int, Int)) => ab._1 - ab._2)
          case Num(n) => AndThen(_ => n)
        }
      }
    }
  }


  sealed trait Cnsl[-A, +B]
  object Cnsl {
    case object GetLine                 extends Cnsl[Unit, String]
    case object PutLine                 extends Cnsl[String, Unit]
    case object GetInt                  extends Cnsl[Unit, Int]
    case object Compute                 extends Cnsl[Unit, Unit]
    case object RepeatN                 extends Cnsl[(String, Int), Unit]
    case class Prompt(message: String)  extends Cnsl[Unit, Unit]
    case class Const[A](value: A)       extends Cnsl[Unit, A]
    case class Dictionary(dict: Map[String, String]) extends Cnsl[String, Option[String]]


    object free {

      val getLine = lift(GetLine)
      val putLine = lift(PutLine)
      val getInt  = lift(GetInt)
      val compute = lift(Compute)
      val repeatN = lift(RepeatN)
      def prompt(message: String) = lift(Prompt(message))
      def const[A](value: A) = lift(Const(value))
      def dictionary(entries: (String, String)*) = lift(Dictionary(entries.toMap))
    }

    object ~~> {

      implicit val function: Cnsl ~~> Function1 = new (Cnsl ~~> Function1) {
        override def apply[A, B](f: Cnsl[A, B]): A => B = f match {
          case Prompt(message) => AndThen(_ => println(message))
          case GetLine => AndThen(_ => StdIn.readLine())
          case GetInt => AndThen(_ => Try(StdIn.readLong().toInt).getOrElse(1))
          case Compute => AndThen(_ => Thread.sleep(1000)) // trivial example
          case PutLine => AndThen(println)
          case RepeatN => AndThen((sn: (String, Int)) => for(_ <- 1 to sn._2) { println(sn._1) })
          case Const(value) => AndThen(_ => value)
          case Dictionary(dict) => AndThen(s => dict.get(s))
        }
      }

      val stubGets: Cnsl ~~> Cnsl = new (Cnsl ~~> Cnsl) {
        def apply[A, B](f: Cnsl[A, B]): Cnsl[A, B] = f match {
          case Cnsl.GetLine => Const("hello")
          case Cnsl.GetInt => Const(1)
          case other => other
        }
      }

      val countPrintlns: Cnsl ~>| Int = new (Cnsl ~>| Int) {
        def apply[A, B](f: Cnsl[A, B]): Int = f match {
          case PutLine | Prompt(_)  => 1
          case _                    => 0
        }
      }
    }
  }
}
