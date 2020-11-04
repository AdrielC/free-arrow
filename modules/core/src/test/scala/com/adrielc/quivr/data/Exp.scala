package com.adrielc.quivr.data

import cats.arrow.ArrowChoice
import com.adrielc.quivr.{liftA2, test}

/** Example of usefulness of abstracting over arrows */
sealed trait Exp
object Exp {
  type Env = Map[String, Val]

  case class Var(name: String)    extends Exp
  case class Add(a: Exp, b: Exp)  extends Exp
  case class If(a: Exp, b: Exp, c: Exp) extends Exp

  sealed trait Val
  case class Num(n: Int) extends Val
  case class Bl(b: Boolean) extends Val

  import cats.implicits._

  def evalExpr[A[_, _]](exp: Exp)(implicit A: ArrowChoice[A]): A[Env, Val] = {
    import A._
    exp match {
      case Var(name) => lift(_(name))
      case Add(a, b) => liftA2(evalExpr[A](a))(evalExpr[A](b))({
        case (Num(a), Num(b)) => Num(a + b)
        case (Bl(a), Bl(b)) => Bl(a && b)
        case (_, a) => a
      })
      case If(a, b, c) => test(evalExpr[A](a) >>> lift({
        case Bl(b) => b
        case Num(n) => n > 0
      })) >>> (evalExpr[A](b) ||| evalExpr[A](c))
    }
  }
}
