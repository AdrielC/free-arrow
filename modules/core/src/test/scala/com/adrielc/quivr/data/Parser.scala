package com.adrielc.quivr.data

import com.adrielc.quivr.ArrowPlus
import com.adrielc.quivr.data.Parser.{DynamicParser, StaticParser}

/**
 * Example of a parser that forms an [[ArrowPlus]] but not an [[cats.arrow.ArrowChoice]]
 */
case class Parser[S, -A, +B](sp: StaticParser[S], dp: DynamicParser[S, A, B]) {
  import Parser._

  def orElse[AA <: A, BB >: B](other: Parser[S, AA, BB]): Parser[S, AA, BB] =
    (this, other) match {
      case (P(SP(empty2, starters2), DP(run2)), P(SP(empty1, starters1), DP(run1))) =>
        P(SP(
          empty1 || empty2,
          starters1 union starters2
        ), DP(
          if(empty1) run2 else run1
        ))
    }

  def compose[AA <: A, BB >: B, C](other: Parser[S, C, AA]): Parser[S, C, BB] =
    (this, other) match {
      case (P(SP(empty2, starters2), DP(run2)), P(SP(empty1, starters1), DP(run1))) =>
        P(SP(
          empty1 && empty2,
          starters1 union (if(empty1) starters2 else Nil)
        ),DP(
          (a, b) => run2.tupled(run1(a, b))
        ))
    }

  def andThen[AA <: A, BB >: B, C](other: Parser[S, BB, C]): Parser[S, AA, C] =
    other compose this

  def first[C]: Parser[S, (A, C), (B, C)] =
    P(sp, DP { case ((a, c), l) =>
      val (b, ll) = dp.run(a, l)
      ((b, c), ll)
    })
}

object Parser {

  def lift[S]: LiftPartiallyApplied[S] = new LiftPartiallyApplied[S]

  class LiftPartiallyApplied[S] {
    def apply[A, B](f: A => B): Parser[S, A, B] = P(SP(empty = true, Nil), DP((a, s) => (f(a), s)))
  }

  case class StaticParser[S](empty: Boolean, symbols: List[S])

  case class DynamicParser[S, -A, +B](run: (A, List[S]) => (B, List[S]))

  def symbol[S, A](s: S): Parser[S, A, A] = P(SP(empty = false, List(s)), DP((a, l) => (a, l :+ s)))

  implicit def arrowPlus[S]: ArrowPlus[Parser[S, *, *]] = new ArrowPlus[Parser[S, *, *]] {

    def lift[A, B](f: A => B): Parser[S, A, B] =
      Parser.lift[S](f)

    def plus[A, B](f: Parser[S, A, B], g: Parser[S, A, B]): Parser[S, A, B] =
      f orElse g

    def compose[A, B, C](f: Parser[S, B, C], g: Parser[S, A, B]): Parser[S, A, C] =
      f compose g

    def zeroArrow[B, C]: Parser[S, B, C] =
      P(SP(empty = true, Nil), DP(zeroArrow.dp.run))

    def first[A, B, C](fa: Parser[S, A, B]): Parser[S, (A, C), (B, C)] =
      fa.first[C]
  }

  private val P = Parser
  private val DP = DynamicParser
  private val SP = StaticParser
}