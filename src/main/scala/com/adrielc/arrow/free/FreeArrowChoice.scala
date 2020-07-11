package com.adrielc.arrow
package free

import cats.arrow.ArrowChoice
import cats.implicits._
import cats.Eval
import com.adrielc.arrow.free.methods.FreeArrowChoiceLike

sealed abstract class FreeArrowChoice[+F[_, _], A, B] extends FreeArrowChoiceLike[FreeArrowChoice, ArrowChoice, F, A, B]

object FreeArrowChoice {

  @inline final def id[A]: FAC[Nothing, A, A] = Id()

  /** Lift a pure function into [[FreeArrowChoice]]. Can be composed with any context */
  @inline final def arr[A, B](f: A => B): FAC[Nothing, A, B] = Arr(cats.data.AndThen(f))

  /** Lift an algebra [[F]] into [[FreeArrowChoice]] */
  @inline final def lift[F[_, _], A, B](fab: F[A, B]): FAC[F, A, B] = Lift(fab)

  @inline final def andThen[F[a, b], A, B, C](first: FAC[F, A, B], next: FAC[F, B, C]): FAC[F, A, C] = (first, next) match {
    case (Arr(f), Arr(g)) => Arr(f andThen g)
    case (Arr(f), AndThen(Arr(g), e)) => AndThen(Arr(f andThen g), e)
    case (AndThen(a, Arr(f)), Arr(g)) => AndThen(a, Arr(f andThen g))
    case (AndThen(a, Arr(f)), AndThen(Arr(g), e)) => AndThen(a, AndThen(Arr(f andThen g), e))
    case _ => AndThen(first, next)
  }

  @inline final def merge[F[_, _], A, B, C](fab: FAC[F, A, B], fac: FAC[F, A, C]): FAC[F, A, (B, C)] = (fab, fac) match {
    case (Arr(f), Arr(g)) => arr(a => (f(a), g(a)))
    case _ => Merge(fab, fac)
  }
  @inline final def first[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, (A, C), (B, C)] = First(fab)
  @inline final def second[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, (C, A), (C, B)] = Second(fab)
  @inline final def split[F[_, _], A, B, C, D](fab: FAC[F, A, B], fcd: FAC[F, C, D]): FAC[F, (A, C), (B, D)] = Split(fab, fcd)
  @inline final def left[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, Either[A, C], Either[B, C]] = Left(fab)
  @inline final def right[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, Either[C, A], Either[C, B]] = Right(fab)
  @inline final def choose[F[_, _], A, B, C, D](fab: FAC[F, A, B], fcd: FAC[F, C, D]): FAC[F, Either[A, C], Either[B, D]] = Choose(fab, fcd)
  @inline final def choice[F[_, _], A, B, C](fab: FAC[F, A, B], fcd: FAC[F, C, B]): FAC[F, Either[A, C], B] = Choice(fab, fcd)

  final implicit def freeArrArrowChoice[F[_, _]]: ArrowChoice[FAC[F, ?, ?]] = new ArrowChoice[FAC[F, ?, ?]] {
    def lift[A, B](f: A => B): FAC[F, A, B] = FAC.arr(f)
    def choose[A, B, C, D](f: FAC[F, A, C])(g: FAC[F, B, D]): FAC[F, Either[A, B], Either[C, D]] = FAC.choose(f, g)
    def first[A, B, C](fa: FAC[F, A, B]): FAC[F, (A, C), (B, C)] = FAC.first(fa)
    def compose[A, B, C](f: FAC[F, B, C], g: FAC[F, A, B]): FAC[F, A, C] = FAC.andThen(g, f)
    override def id[A]: FAC[F, A, A] = FAC.id
    override def rmap[A, B, C](fab: FAC[F, A, B])(f: B => C): FAC[F, A, C] = andThen(fab, lift(f))
    override def lmap[A, B, C](fab: FAC[F, A, B])(f: C => A): FAC[F, C, B] = compose(fab, lift(f))
    override def second[A, B, C](fa: FAC[F, A, B]): FAC[F, (C, A), (C, B)] = FAC.second(fa)
    override def choice[A, B, C](f: FAC[F, A, C], g: FAC[F, B, C]): FAC[F, Either[A, B], C] = FAC.choice(f, g)
    override def split[A, B, C, D](f: FAC[F, A, B], g: FAC[F, C, D]): FAC[F, (A, C), (B, D)] = FAC.split(f, g)
    override def merge[A, B, C](f: FAC[F, A, B], g: FAC[F, A, C]): FAC[F, A, (B, C)] = FAC.merge(f, g)
    override def left[A, B, C](fab: FAC[F, A, B]): FAC[F, Either[A, C], Either[B, C]] = FAC.left(fab)
    override def right[A, B, C](fab: FAC[F, A, B]): FAC[F, Either[C, A], Either[C, B]] = FAC.right(fab)
  }

  final private case class Id[A]() extends FAC[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowChoice[G]): G[A, A] = A.id
  }
  final private case class Arr[A, B](f: cats.data.AndThen[A, B]) extends FAC[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowChoice[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FAC[F, A, B], end: FAC[F, B, C]) extends FAC[F, A, C] {
    type EvalG[G[_, _], X, Y] = Eval[G[X, Y]]

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, C] = {
      type EvalG[X, Y] = Eval[G[X, Y]]
      lazy val lazyAnd = new (FAC[F, ?, ?] ~~> EvalG) {
        def apply[D, E](f: FAC[F, D, E]): EvalG[D, E] = f match {
          case a: AndThen[F, D, b, E] =>

            for {
              b <- Eval.later(apply(a.begin)).flatten
              e <- apply(a.end)
            } yield b.andThen(e)

          case _ => Eval.now(f.foldMap(fk))
        }
      }

      val eval = for {
        e <- lazyAnd(begin)
        b <- Eval.later(lazyAnd(end)).flatten
      } yield e andThen b
      eval.value
    }
  }
  final private case class Lift[F[_, _], A, B](fab: F[A, B]) extends FAC[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, B] = fk(fab)
  }
  final private case class Merge[F[_, _], A, B, C](_first: FAC[F, A, B], _second: FAC[F, A, C]) extends FAC[F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, (B, C)] = A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class Split[F[_, _], A, B, C, D](_first: FAC[F, A, B], _second: FAC[F, C, D]) extends FAC[F, (A, C), (B, D)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(A, C), (B, D)] = A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class First[F[_, _], A, B, C](_first: FAC[F, A, B]) extends FAC[F, (A, C), (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(A, C), (B, C)] = A.first(_first.foldMap(fk))
  }
  final private case class Second[F[_, _], A, B, C](_second: FAC[F, A, B]) extends FAC[F, (C, A), (C, B)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }
  final private case class Choose[F[_, _], A, B, C, D](_left: FAC[F, A, B], _right: FAC[F, C, D]) extends FAC[F, Either[A, C], Either[B, D]] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], Either[B, D]] = A.choose(_left.foldMap(fk))(_right.foldMap(fk))
  }
  final private case class Left[F[_, _], A, B, C](_left: FAC[F, A, B]) extends FAC[F, Either[A, C], Either[B, C]] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], Either[B, C]] = A.left(_left.foldMap(fk))
  }
  final private case class Right[F[_, _], A, B, C](_right: FAC[F, A, B]) extends FAC[F, Either[C, A], Either[C, B]] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[C, A], Either[C, B]] = A.right(_right.foldMap(fk))
  }
  final private case class Choice[F[_, _], A, B, C](_left: FAC[F, A, B], _right: FAC[F, C, B]) extends FAC[F, Either[A, C], B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], B] = A.choice(_left.foldMap(fk), _right.foldMap(fk))
  }
}