package com.adrielc.arrow
package free

import cats.arrow.Arrow
import cats.implicits._
import cats.Eval
import com.adrielc.arrow.free.methods.FreeArrowLike

sealed abstract class FreeArrow[+F[_, _], A, B] extends FreeArrowLike[FreeArrow, Arrow, F, A, B] {

  def choice: FreeArrowChoice[F, A, B] = FreeArrow.arrowToChoice(this)
}


object FreeArrow { self =>

  @inline final def id[A]: FA[Nothing, A, A] = Id()

  /** Lift a pure function into [[FreeArrow]]. Can be composed with any context */
  @inline final def arr[A, B](f: A => B): FA[Nothing, A, B] = Arr(cats.data.AndThen(f))

  /** Lift an algebra [[F]] into [[FreeArrow]] */
  @inline final def lift[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = Lift(fab)

  @inline final def andThen[F[a, b], A, B, C](first: FA[F, A, B], next: FA[F, B, C]): FA[F, A, C] = (first, next) match {
    case (Arr(f), Arr(g)) => Arr(f andThen g)
    case (Arr(f), AndThen(Arr(g), e)) => AndThen(Arr(f andThen g), e)
    case (AndThen(a, Arr(f)), Arr(g)) => AndThen(a, Arr(f andThen g))
    case (AndThen(a, Arr(f)), AndThen(Arr(g), e)) => AndThen(a, AndThen(Arr(f andThen g), e))
    case _ => AndThen(first, next)
  }

  @inline final def merge[F[_, _], A, B, C](fab: FA[F, A, B], fac: FA[F, A, C]): FA[F, A, (B, C)] = (fab, fac) match {
    case (Arr(f), Arr(g)) => arr(a => (f(a), g(a)))
    case _ => Merge(fab, fac)
  }

  @inline final def rmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = andThen(fab, arr(f))
  @inline final def lmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = andThen(arr(f), fab)
  @inline final def first[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (A, C), (B, C)] = First(fab)
  @inline final def second[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (C, A), (C, B)] = Second(fab)
  @inline final def split[F[_, _], A, B, C, D](fab: FA[F, A, B], fcd: FA[F, C, D]): FA[F, (A, C), (B, D)] = Split(fab , fcd)

  final implicit def freeArrArrow[F[_, _]]: Arrow[FA[F, ?, ?]] = new Arrow[FA[F, ?, ?]] {
    def lift[A, B](f: A => B): FA[F, A, B] = FA.arr(f)
    def compose[A, B, C](f: FA[F, B, C], g: FA[F, A, B]): FA[F, A, C] = FA.andThen(g, f)
    def first[A, B, C](fa: FA[F, A, B]): FA[F, (A, C), (B, C)] = FA.first(fa)
    override def second[A, B, C](fa: FA[F, A, B]): FA[F, (C, A), (C, B)] = FA.second(fa)
    override def split[A, B, C, D](f: FA[F, A, B], g: FA[F, C, D]): FA[F, (A, C), (B, D)] = FA.split(f, g)
    override def merge[A, B, C](f: FA[F, A, B], g: FA[F, A, C]): FA[F, A, (B, C)] = FA.merge(f, g)
    override def rmap[A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = andThen(fab, lift(f))
    override def lmap[A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = compose(fab, lift(f))
  }

  final private case class Id[A]() extends FA[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, A] = A.id
  }
  final private case class Arr[A, B](f: cats.data.AndThen[A, B]) extends FA[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FA[F, A, B], end: FA[F, B, C]) extends FA[F, A, C] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, C] = {
      type EvalG[X, Y] = Eval[G[X, Y]]
      lazy val lazyAnd = new (FA[F, ?, ?] ~~> EvalG) {
        def apply[D, E](f: FA[F, D, E]): EvalG[D, E] = f match {
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
  final private case class Lift[F[_, _], A, B](fab: F[A, B]) extends FA[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, B] = fk(fab)
  }
  final private case class Merge[F[_, _], A, B, C](_first: FA[F, A, B], _second: FA[F, A, C]) extends FA[F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, (B, C)] = A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class Split[F[_, _], A, B, C, D](_first: FA[F, A, B], _second: FA[F, C, D]) extends FA[F, (A, C), (B, D)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(A, C), (B, D)] = A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class First[F[_, _], A, B, C](_first: FA[F, A, B]) extends FA[F, (A, C), (B, C)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(A, C), (B, C)] = A.first(_first.foldMap(fk))
  }
  final private case class Second[F[_, _], A, B, C](_second: FA[F, A, B]) extends FA[F, (C, A), (C, B)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }

  private def arrowToChoice[F[_, _]]: FA[F, ?, ?] ~~> FreeArrowChoice[F, ?, ?] =
    new (FA[F, ?, ?] ~~> FreeArrowChoice[F, ?, ?]) { self =>
      def apply[A, B](f: FA[F, A, B]): FreeArrowChoice[F, A, B] = f match {
        case _: Id[a]                     => FAC.id[a]
        case Arr(f)                       => FAC.arr(f)
        case l: Lift[F, A, B]             => FAC.lift(l.fab)
        case a: AndThen[F, A, b, B]       => FAC.andThen(self(a.begin), self(a.end))
        case m: Merge[f, a, b, c]         => FAC.merge[f, a, b, c](self(m._first), self(m._second))
        case fst: First[f, a, b, c]       => FAC.first[f, a, b, c](self(fst._first))
        case s: Second[f, a, b, c]        => FAC.second[f, a, b, c](self(s._second))
        case s: Split[f, a, b, c, d]      => FAC.split[f, a, b, c, d](self(s._first), self(s._second))
      }
    }
}
