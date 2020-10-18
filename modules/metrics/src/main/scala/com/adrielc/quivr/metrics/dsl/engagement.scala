package com.adrielc.quivr.metrics
package dsl

import cats.data.{Kleisli, NonEmptyList}
import matryoshka.Recursive
import matryoshka.data.Fix
import scalaz.Functor
import eu.timepit.refined.auto._
import cats.implicits._
import cats.kernel.{CommutativeSemigroup => CSemi}
import com.adrielc.quivr.free.{FA, FAP}
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data.{Engagement, KeyCounts}
import com.adrielc.quivr.metrics.dsl.engagement.ExprF.{And, Binary, Count, Div, Eq, IfThen, Mult, Or, Sign, Sum, Value}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp.{EngagementToJudgement, EngagementToLabel}
import com.adrielc.quivr.metrics.result.Engagements
import function.Eq.double._

object engagement {
  import Rec.ops._

  def toLabel[A]: EngagementLabelBuilder[A] = new EngagementLabelBuilder[A]

  def toJudgement[A]: EngagementJudgeBuilder[A] = new EngagementJudgeBuilder[A]

  class FixEngOps[E](private val fix: Expr[E]) extends AnyVal {
    def +[B: Rec[*, E]](other: B) : Expr[E] = sum(fix, other.embed)
    def *[B: Rec[*, E]](other: B) : Expr[E] = times(fix, other.embed)
    def /[B: Rec[*, E]](other: B) : Expr[E] = div(fix, other.embed)
    def |[B: Rec[*, E]](other: B) : Expr[E] = or(fix, other.embed)
    def &&[B: Rec[*, E]](other: B): Expr[E] = and(fix, other.embed)
    def <=[B: Rec[*, E]](b: B)    : Expr[E] = ltEq(fix, b.embed)
    def >=[B: Rec[*, E]](b: B)    : Expr[E] = gtEq(fix, b.embed)
    def >[B: Rec[*, E]](b: B)     : Expr[E] = gt(fix, b.embed)
    def <[B: Rec[*, E]](b: B)     : Expr[E] = lt(fix, b.embed)
    def ===[B: Rec[*, E]](b: B)   : Expr[E] = isEq(fix, b.embed)
    def unary_!                   : Expr[E] = binary(fix)
    def sig                       : Expr[E] = sign(fix)
    def unary_-                   : Expr[E] = fix * -1
    def isPos                     : Expr[E] = fix > 0
  }

  def value[E](v: Double)                           : Expr[E] = Fix(Value(v): ExprF[E, Expr[E]])
  def count[E](e: E)                                : Expr[E] = Fix(Count(e): ExprF[E, Expr[E]])
  def isPos[E](e: E)                                : Expr[E] = count(e) > 0
  def sum[E](e1: Expr[E], e2: Expr[E])              : Expr[E] = Fix(Sum(e1, e2))
  def sum[E](e: Expr[E], es: Expr[E]*)            : Expr[E] = es.foldLeft(e)(_ + _)
  def weightedSum[E](w: (Expr[E], Double), ws: (Expr[E], Double)*): Expr[E] = sum(w._1 * w._2, ws.map { case (e, w) => e * w}:_*)
  def binary[E](e1: Expr[E])                        : Expr[E] = Fix(Binary(e1))
  def times[E](e1: Expr[E], e2: Expr[E])            : Expr[E] = Fix(Mult(e1, e2))
  def div[E](e1: Expr[E], e2: Expr[E])              : Expr[E] = Fix(Div(e1, e2))
  def or[E](e1: Expr[E], e2: Expr[E])               : Expr[E] = Fix(Or(e1, e2))
  def and[E](e1: Expr[E], e2: Expr[E])              : Expr[E] = Fix(And(e1, e2))
  def isEq[E](e1: Expr[E], eq: Expr[E])             : Expr[E] = Fix(Eq(===, e1, eq))
  def ltEq[E](e1: Expr[E], ltEq: Expr[E])           : Expr[E] = Fix(Eq(<=, e1, ltEq))
  def lt[E](e1: Expr[E], lt: Expr[E])               : Expr[E] = Fix(Eq(<, e1, lt))
  def gtEq[E](e1: Expr[E], gtEq: Expr[E])           : Expr[E] = Fix(Eq(>=, e1, gtEq))
  def gt[E](e1: Expr[E], gt: Expr[E])               : Expr[E] = Fix(Eq(>, e1, gt))
  def ifThen[E, A: Rec[*, E]](i: Expr[E], t: A)     : Expr[E] = Fix(IfThen(i, t.embed))
  def sign[E](e: Expr[E])                           : Expr[E] = Fix(Sign(e))

  type Expr[E] = Fix[ExprF[E, +*]]
  sealed trait ExprF[+E, +A] {
    import ExprF._
    def map[B](f: A => B): ExprF[E, B] = this match {
      case Count(e)         => Count(e)
      case Value(value)     => Value(value)
      case Binary(exp)      => Binary(f(exp))
      case Mult(e1, e2)     => Mult(f(e1), f(e2))
      case Sum(e1, e2)      => Sum(f(e1), f(e2))
      case IfThen(e1, e2)   => IfThen(f(e1), f(e2))
      case Div(exp1, exp2)  => Div(f(exp1), f(exp2))
      case Or(exp1, exp2)   => Or(f(exp1), f(exp2))
      case And(exp1, exp2)  => And(f(exp1), f(exp2))
      case Eq(e, exp1, exp2)=> Eq(e, f(exp1), f(exp2))
      case Sign(e)          => Sign(f(e))
    }
  }
  object ExprF {
    import matryoshka.implicits._

    case class Count[E](e: E)                   extends ExprF[E, Nothing]
    case class Value[E](value: Double)          extends ExprF[Nothing, Nothing]
    case class Binary[E, A](exp: A)             extends ExprF[E, A]
    case class Mult[E, A](e1: A, e2: A)         extends ExprF[E, A]
    case class Sum[E, A](e1: A, e2: A)          extends ExprF[E, A]
    case class Div[E, A](e1: A, e2: A)          extends ExprF[E, A]
    case class Or[E, A](e1: A, e2: A)           extends ExprF[E, A]
    case class And[E, A](e1: A, e2: A)          extends ExprF[E, A]
    case class IfThen[E, A](i: A, t: A)         extends ExprF[E, A]
    case class Sign[E, A](e: A)                 extends ExprF[E, A]
    case class Eq[E, A](eq: function.Eq[Double], e1: A, e2: A)  extends ExprF[E, A]

    private[dsl] def evalToKleisli[T, E](e: T)(implicit T: Recursive.Aux[T, ExprF[E, *]]): Kleisli[Option, KeyCounts[E], Double] =
      e.cata[Kleisli[Option, KeyCounts[E], Double]] {
        case Value(value)     => Kleisli.pure(value)
        case Count(e)         => Kleisli(_.counts.lookup(e).map(_.toDouble))
        case Sign(e)          => e.map(_.signum.toDouble)
        case Binary(e1)       => e1.local(_.binarize)
        case IfThen(e1, e2)   => Kleisli(a => if(e1(a).isDefined) e2(a) else None)
        case Or(e1, e2)       => Kleisli(a => e1(a).orElse(e2(a)))
        case And(e1, e2)      => Kleisli(a => (e1(a), e2(a)).mapN(_ + _))
        case Sum(e1, e2)      => e1 |+| e2
        case Mult(e1, e2)     => mult(e1, e2)
        case Div(num, den)    => Kleisli(e => den(e).map(d => if(d == 0) 0.0 else num(e).foldMap(_ / d)))
        case Eq(e, exp, eq)   => Kleisli(eng => exp(eng) >>= (a => eq(eng) >>= (b => e(a, b).guard[Option].as(a))))
      }

    private def mult[E] =
      cats.data.Kleisli.catsDataMonoidForKleisli[Option, KeyCounts[E], Double](
        cats.instances.option.catsKernelStdCommutativeMonoidForOption[Double](CSemi.instance(_ * _))).combine _

    implicit def fun[E]: Functor[ExprF[E, *]] = new Functor[ExprF[E, *]] {
      override def map[A, B](fa: ExprF[E, A])(f: A => B): ExprF[E, B] = fa.map(f)
    }
  }


  trait ExprOps {
    implicit def toExprOps[E](e: Expr[E]): FixEngOps[E] = new FixEngOps(e)
  }
  trait Rec[A, E] {
    
    def embed(a: A): Expr[E]
  }
  private object Rec {
    implicit def liftFixExpr[E]: Rec[Expr[E], E] = (a: Expr[E]) => a
    implicit def liftDouble[E]: Rec[Double, E] = a => engagement.value(a)
    implicit def liftInt[E]: Rec[Int, E] = a => engagement.value(a.toDouble)
    implicit val liftEngagement: Rec[Engagement, Engagement] = a => engagement.count(a)

    object ops {
      implicit class RecOps[A](private val a: A) extends AnyVal {
        def embed[E](implicit R: Rec[A, E]): Expr[E] = R.embed(a)
      }
    }
  }



  private[dsl] trait EngBuilder[F[_, _],  A, B] {
    def create[E](e: dsl.engagement.Expr[E])(implicit E: Engagements[A, E]): F[A, B]
    def apply[E](e: dsl.engagement.Expr[E])(implicit E: Engagements[A, E]): FA[F, A, B] = FA.liftK(create(e))
    def apply[E](e: dsl.engagement.Expr[E], es: dsl.engagement.Expr[E]*)(implicit E: Engagements[A, E]): FAP[F, A, B] =
      FA.plus(NonEmptyList(apply(e), es.map(apply(_)).toList))
  }
  class EngagementJudgeBuilder[A] private[dsl] extends EngBuilder[EngagementOp, A, WithGroundTruth[A]] { self =>

    def create[E](e: Expr[E])(implicit E: Engagements[A, E]): EngagementOp[A, WithGroundTruth[A]] = EngagementToJudgement(e)
  }
  class EngagementLabelBuilder[A] private[dsl] extends EngBuilder[EngagementOp, A, WithLabels[A]] { self =>

    def create[E](e: Expr[E])(implicit E: Engagements[A, E]): EngagementOp[A, WithLabels[A]] = EngagementToLabel(e)
  }
}
