package com.adrielc.quivr
package data

import scala.io.StdIn
import scala.util.Try

object exampleDsl {
  import com.adrielc.quivr.free.FreeArrow.liftK

  sealed trait Expr[A, B] extends Product with Serializable
  object Expr {

    final case object Add extends Expr[(Int, Int), Int]
    final case object Sub extends Expr[(Int, Int), Int]
    final case class Num(n: Int) extends Expr[Unit, Int]

    object free {
      val add = liftK(Add)
      val sub = liftK(Sub)
      def num(n: Int) = liftK(Num(n))
    }

    object ~~> {

      val function = new Pure[Expr] {
        def toFn[A, B](f: Expr[A, B]): A => B = f match {
          case Add => (ab: (Int, Int)) => ab._1 + ab._2
          case Sub => (ab: (Int, Int)) => ab._1 - ab._2
          case Num(n) => _ => n
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

      val getLine = liftK(GetLine)
      val putLine = liftK(PutLine)
      val getInt  = liftK(GetInt)
      val compute = liftK(Compute)
      val repeatN = liftK(RepeatN)
      def prompt(message: String) = liftK(Prompt(message))
      def const[A](value: A) = liftK(Const(value))
      def dictionary(entries: (String, String)*) = liftK(Dictionary(entries.toMap))
    }

    object ~~> {

      implicit val function: Cnsl ~~> Function1 = new Pure[Cnsl] {
        override def toFn[A, B](f: Cnsl[A, B]): A => B = f match {
          case Prompt(message) => _ => println(message)
          case GetLine => _ => StdIn.readLine()
          case GetInt => _ => Try(StdIn.readLong().toInt).getOrElse(1)
          case Compute => _ => Thread.sleep(1000) // trivial example
          case PutLine => println
          case RepeatN => (sn: (String, Int)) => for(_ <- 1 to sn._2) { println(sn._1) }
          case Const(value) => _ => value
          case Dictionary(dict) => s => dict.get(s)
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
