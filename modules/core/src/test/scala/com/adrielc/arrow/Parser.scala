package com.adrielc.arrow

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.Parser._

case class Parser[S, A, B](sp: StaticParser[S], dp: DynamicParser[S, A, B])

object Parser {
  val P = Parser
  val DP = DynamicParser
  val SP = StaticParser

  case class StaticParser[S](empty: Boolean, symbols: List[S])

  case class DynamicParser[S, A, B](run: (A, List[S]) => (B, List[S]))

  def symbol[S, A](s: S): Parser[S, A, A] = P(SP(empty = false, List(s)), DP((a, l) => (a, l :+ s)))

  implicit def arrowPlus[S]: ArrowPlus[Parser[S, ?, ?]] = new ArrowPlus[Parser[S, ?, ?]] {

    def lift[A, B](f: A => B): Parser[S, A, B] = P(SP(empty = true, Nil), DP((a, s) => (f(a), s)))

    def plus[A, B](f: Parser[S, A, B], g: Parser[S, A, B]): Parser[S, A, B] =
      (f, g) match {
        case (P(SP(empty2, starters2), DP(run2)), P(SP(empty1, starters1), DP(run1))) =>
          P(SP(
            empty1 || empty2,
            starters1 union starters2
          ),DP(
            if(empty1) run2 else run1
          ))
      }

    def compose[A, B, C](f: Parser[S, B, C], g: Parser[S, A, B]): Parser[S, A, C] =
      (f, g) match {
        case (P(SP(empty2, starters2), DP(run2)), P(SP(empty1, starters1), DP(run1))) =>
          P(SP(
            empty1 && empty2,
            starters1 union (if(empty1) starters2 else Nil)
          ),DP(
            (a, b) => run2.tupled(run1(a, b))
          ))
      }

    def zeroArrow[B, C]: Parser[S, B, C] = P(SP(empty = true, Nil), DP((a, b) => (a.asInstanceOf[C], b)))

    def first[A, B, C](fa: Parser[S, A, B]): Parser[S, (A, C), (B, C)] = ???
  }
}

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

  def test[A[_, _], B](a: A[B, Boolean])(implicit A: Arrow[A]): A[B, Either[B, B]] =
    (a &&& A.id) >>> A.lift { case (b, x) => if(b) Left(x) else Right(x) }


  def eval[A[_, _]](exp: Exp)(implicit A: ArrowChoice[A]): A[Env, Val] = {
    import A._
    exp match {
      case Var(name) => lift(_(name))
      case Add(a, b) => liftA2(eval[A](a))(eval[A](b))({
        case (Num(a), Num(b)) => Num(a + b)
        case (Bl(a), Bl(b)) => Bl(a && b)
        case (_, a) => a
      })
      case If(a, b, c) => test(eval[A](a) >>> lift({
        case Bl(b) => b
        case Num(n) => n > 0
      })) >>> (eval[A](b) ||| eval[A](c))
    }
  }
}
