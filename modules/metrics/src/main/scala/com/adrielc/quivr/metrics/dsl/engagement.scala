package com.adrielc.quivr.metrics
package dsl

import matryoshka.data.Fix
import scalaz.Functor

object engagement {
  import LabelerF._
  import JudgeF._
  import function.double

  /** Expression used to define mapping of result engagements [[E]] to a relevance label (continuous) **/
  type Labeler[E] = Fix[LabelerF[E, *]]
  object Labeler {
    def countOf[E](e: E)                          : Labeler[E] = Fix(Count(e): LabelerF[E, Labeler[E]])
    def value[E](v: Double)                       : Labeler[E] = Fix(Value(v): LabelerF[E, Labeler[E]])
    def sum[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(Sum(e1, e2))
    def times[E](e1: Labeler[E], e2: Labeler[E])  : Labeler[E] = Fix(Mult(e1, e2))
    def div[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(Div(e1, e2))
    def ifThen[E](i: Judge[E], t: Labeler[E])     : Labeler[E] = Fix(IfThen(i, t))

    // label filters
    def and[E](e1: Labeler[E], e2: Labeler[E])    : Labeler[E] = Fix(And(e1, e2))
    def or[E](e1: Labeler[E], e2: Labeler[E])     : Labeler[E] = Fix(Or(e1, e2))
    def isEq[E](e: Labeler[E], v: Labeler[E])     : Labeler[E] = Fix(Equiv(double.===, e, v): LabelerF[E, Labeler[E]])
    def ltEq[E](e: Labeler[E], v: Labeler[E])     : Labeler[E] = Fix(Equiv(double.<=, e, v): LabelerF[E, Labeler[E]])
    def lt[E](e: Labeler[E], v: Labeler[E])       : Labeler[E] = Fix(Equiv(double.<, e, v): LabelerF[E, Labeler[E]])
    def gtEq[E](e: Labeler[E], v: Labeler[E])     : Labeler[E] = Fix(Equiv(double.>=, e, v): LabelerF[E, Labeler[E]])
    def gt[E](e: Labeler[E], v: Labeler[E])       : Labeler[E] = Fix(Equiv(double.>, e, v): LabelerF[E, Labeler[E]])
  }


  /** Expression used to define mapping of result engagements [[E]] to a judgement (binary) **/
  type Judge[E]  = Fix[JudgeF[E, *]]
  object Judge {
    def isEq[E](e: Labeler[E], v: Labeler[E]) : Judge[E] = Fix(Equiv(double.===, e, v): JudgeF[E, Judge[E]])
    def ltEq[E](e: Labeler[E], v: Labeler[E]) : Judge[E] = Fix(Equiv(double.<=, e, v): JudgeF[E, Judge[E]])
    def lt[E](e: Labeler[E], v: Labeler[E])   : Judge[E] = Fix(Equiv(double.<, e, v): JudgeF[E, Judge[E]])
    def gtEq[E](e: Labeler[E], v: Labeler[E]) : Judge[E] = Fix(Equiv(double.>=, e, v): JudgeF[E, Judge[E]])
    def gt[E](e: Labeler[E], v: Labeler[E])   : Judge[E] = Fix(Equiv(double.>, e, v): JudgeF[E, Judge[E]])
    def and[E](e1: Judge[E], e2: Judge[E])    : Judge[E] = Fix(And(e1, e2))
    def or[E](e1: Judge[E], e2: Judge[E])     : Judge[E] = Fix(Or(e1, e2))
  }


  private[dsl] object LabelerF {
    case class Count[E](e: E)                   extends LabelerF[E, Nothing]
    case class Value(value: Double)             extends LabelerF[Nothing, Nothing]
    case class Mult[E, A](e1: A, e2: A)         extends LabelerF[E, A]
    case class Sum[E, A](e1: A, e2: A)          extends LabelerF[E, A]
    case class Div[E, A](e1: A, e2: A)          extends LabelerF[E, A]
    case class IfThen[E, A](i: Judge[E], t: A)  extends LabelerF[E, A]


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
      case IfThen(e1, e2)   => IfThen(e1, f(e2))
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

  private[dsl] trait LabelFor[A, E] { def embed(a: A): Labeler[E] }
  private[dsl] object LabelFor {
    implicit def liftFixExpr[E]: Labeler[E] LabelFor E = (a: Labeler[E]) => a
    implicit def liftInt[E]: Int LabelFor E = a => Fix(Value(a.toDouble): LabelerF[E, Labeler[E]])
    implicit def liftDouble[E]: Double LabelFor E = a => Fix(Value(a): LabelerF[E, Labeler[E]])
  }
  private[dsl] class RecOps[A](private val a: A) extends AnyVal {
    def labeler[E](implicit R: LabelFor[A, E]): Labeler[E] = R.embed(a)
  }
}
