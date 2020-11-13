package com.adrielc.quivr.metrics
package dsl

import com.adrielc.quivr.metrics.function.Eq
import matryoshka.data.Fix
import scalaz.Functor

import Numeric.Implicits._

object engagement {
  import LabelerF._
  import JudgeF._

  /** Expression used to define mapping of result engagements [[E]] to a relevance label (continuous) **/
  type Labeler[E] = Fix[LabelerF[E, *]]
  object Labeler {
    def countOf[E](e: E)                          : Labeler[E] = Fix(Count(e): LabelerF[E, Labeler[E]])
    def value[E](v: Double)                       : Labeler[E] = Fix(Value(v): LabelerF[E, Labeler[E]])
    def sum[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(Sum(e1, e2))
    def log[E](e1: Labeler[E], base: Double)      : Labeler[E] = Fix(Log(e1, base))
    def times[E](e1: Labeler[E], e2: Labeler[E])  : Labeler[E] = Fix(Mult(e1, e2))
    def div[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(Div(e1, e2))
    def as[E](i: Judge[E], t: Labeler[E])         : Labeler[E] = Fix(As(i, t))
    def and[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(And(e1, e2))
    def or[E](e1: Labeler[E], e2: Labeler[E])     : Labeler[E] = Fix(Or(e1, e2))
    def equiv[E](eq: Eq[Double], e: Labeler[E], v: Labeler[E]): Labeler[E] = Fix(Equiv(eq, e, v): LabelerF[E, Labeler[E]])
  }


  /** Expression used to define mapping of result engagements [[E]] to a judgement (binary) **/
  type Judge[E]  = Fix[JudgeF[E, *]]
  object Judge {
    def and[E](e1: Judge[E], e2: Judge[E]): Judge[E] = Fix(And(e1, e2))
    def or[E](e1: Judge[E], e2: Judge[E]): Judge[E] = Fix(Or(e1, e2))
    def equiv[E](eq: Eq[Double], e: Labeler[E], v: Labeler[E]): Judge[E] = Fix(Equiv(eq, e, v): JudgeF[E, Judge[E]])
  }


  private[dsl] object LabelerF {
    case class Count[E](e: E)                   extends LabelerF[E, Nothing]
    case class Value(value: Double)             extends LabelerF[Nothing, Nothing]
    case class Mult[E, A](e1: A, e2: A)         extends LabelerF[E, A]
    case class Sum[E, A](e1: A, e2: A)          extends LabelerF[E, A]
    case class Log[E, A](e1: A, base: Double)   extends LabelerF[E, A]
    case class Div[E, A](e1: A, e2: A)          extends LabelerF[E, A]
    case class As[E, A](i: Judge[E], t: A)      extends LabelerF[E, A]


    implicit def functorLabelerF[E]: Functor[LabelerF[E, *]] =
      new Functor[LabelerF[E, *]] { def map[A, B](fa: LabelerF[E, A])(f: A => B): LabelerF[E, B] = fa.map(f) }
  }
  sealed private[dsl] trait LabelerF[+E, +A] {
    import LabelerF._
    def map[B](f: A => B): LabelerF[E, B] = this match {
      case Count(e)         => Count(e)
      case Value(value)     => Value(value)
      case Mult(e1, e2)     => Mult(f(e1), f(e2))
      case Sum(e1, e2)      => Sum(f(e1), f(e2))
      case Log(e1, base)    => Log(f(e1), base)
      case As(e1, e2)       => As(e1, f(e2))
      case Div(exp1, exp2)  => Div(f(exp1), f(exp2))
      case j: JudgeF[E, A]  => j.map(f)
    }
  }

  private[dsl] object JudgeF {

    case class Equiv[E, A](eq: Eq[Double], a: Labeler[E], b: Labeler[E]) extends JudgeF[E, A]
    case class Or[E, A](e1: A, e2: A) extends JudgeF[E, A]
    case class And[E, A](e1: A, e2: A) extends JudgeF[E, A]

    implicit def functorJudgeF[E]: Functor[JudgeF[E, *]] =
      new Functor[JudgeF[E, *]] { def map[A, B](fa: JudgeF[E, A])(f: A => B): JudgeF[E, B] = fa.map(f) }
  }
  private[dsl] sealed trait JudgeF[+E, +A] extends LabelerF[E, A] {
    override def map[B](f: A => B): JudgeF[E, B] = this match {
      case Equiv(eq, e, v) => Equiv(eq, e, v)
      case Or(exp1, exp2) => Or(f(exp1), f(exp2))
      case And(exp1, exp2) => And(f(exp1), f(exp2))
    }
  }

  private[dsl] trait LabelFor[A, E] { def labeler(a: A): Labeler[E] }
  private[dsl] object LabelFor {
    implicit def liftFixExpr[E]: Labeler[E] LabelFor E = (a: Labeler[E]) => a
    implicit def liftNumeric[N: Numeric, E]: N LabelFor E = a => Fix(Value(a.toDouble): LabelerF[E, Labeler[E]])
  }
  private[dsl] implicit class LabelForOps[A](private val a: A) extends AnyVal {
    def labeler[E](implicit R: LabelFor[A, E]): Labeler[E] = R.labeler(a)
  }
}
