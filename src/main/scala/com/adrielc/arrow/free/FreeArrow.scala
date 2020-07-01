package com.adrielc.arrow
package free

import cats.arrow.Arrow
import cats.data.{AndThen => Then}
import cats.implicits._
import cats.{Applicative, Eval, Monoid}
import com.adrielc.arrow.data.ConstArr

sealed abstract class FreeArrow[+F[_, _], A, B]
  extends FreeArrowLike[FreeArrow, F, A, B] {
  import FreeArrow.{lift, arrowToChoice}

  /** Interprets/Runs the sequence of operations using the semantics of `Arrow[G]` */
  def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, B]

  def analyze[M: Monoid](fm: F ~~> λ[(α, β) => M]): M =
    foldMap(new (F ~~> ConstArr[M, ?, ?]) {
      def apply[C, D](f: F[C, D]): ConstArr[M, C, D] = ConstArr(fm(f))
    }).getConst

  def compile[G[_, _]](fg: F ~~> G): FreeArrow[G, A, B] =
    foldMap(new (F ~~> FreeArrow[G, ?, ?]) {
      def apply[C, D](f: F[C, D]): FreeArrow[G, C, D] = lift(fg(f))
    })

  def choice: FreeArrowChoice[F, A, B] = arrowToChoice(this)
}


object FreeArrow { self =>
  type FA[F[_, _], A, B] = FreeArrow[F, A, B]

  def apply[A]: FA[Nothing, A, A] = id

  final def id[A]: FA[Nothing, A, A] = Id()

  /** Lift a pure function into [[FA]]. Can be composed with any context */
  final def arr[A, B](f: A => B): FA[Nothing, A, B] = Arr(Then(f))

  /** Lift a pure function into [[FA]]. Can be composed with any context */
  final def lift[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = Lift(fab)

  final def andThen[F[a, b], A, B, C](first: FA[F, A, B], next: FA[F, B, C]): FA[F, A, C] = (first, next) match {
    case (Arr(f), Arr(g)) => Arr(f andThen g)
    case (Arr(f), AndThen(Arr(g), e)) => AndThen(Arr(f andThen g), e)
    case (AndThen(a, Arr(f)), Arr(g)) => AndThen(a, Arr(f andThen g))
    case (AndThen(a, Arr(f)), AndThen(Arr(g), e)) => AndThen(a, AndThen(Arr(f andThen g), e))
    case _ => AndThen(first, next)
  }

  final def rmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = fab match {
    case Arr(g) => Arr(g andThen f)
    case AndThen(a, Arr(g)) => AndThen(a, Arr(g andThen f))
    case _ => AndThen(fab, arr(f))
  }

  final def lmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = fab match {
    case Arr(g) => Arr(g compose f)
    case AndThen(Arr(g), a) => AndThen(Arr(g compose f), a)
    case _ => AndThen(arr(f), fab)
  }

  final def first[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (A, C), (B, C)] = First(fab)

  final def second[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (C, A), (C, B)] = Second(fab)

  final def split[F[_, _], A, B, C, D](fab: FA[F, A, B], fcd: FA[F, C, D]): FA[F, (A, C), (B, D)] = Split(fab , fcd)

  final def merge[F[_, _], A, B, C](fab: FA[F, A, B], fac: FA[F, A, C]): FA[F, A, (B, C)] = (fab, fac) match {
    case (Arr(f), Arr(g)) => Arr(Then(a => (f(a), g(a))))
    case _ => Merge(fab, fac)
  }

  final implicit def freeArrArrow[F[_, _]]: Arrow[FA[F, ?, ?]] = new Arrow[FA[F, ?, ?]] {
    def lift[A, B](f: A => B): FA[F, A, B] = self.arr(f)
    def compose[A, B, C](f: FA[F, B, C], g: FA[F, A, B]): FA[F, A, C] = self.andThen(g, f)
    def first[A, B, C](fa: FA[F, A, B]): FA[F, (A, C), (B, C)] = self.first(fa)
    override def second[A, B, C](fa: FA[F, A, B]): FA[F, (C, A), (C, B)] = self.second(fa)
    override def rmap[A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = self.rmap(fab)(f)
    override def lmap[A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = self.lmap(fab)(f)
    override def split[A, B, C, D](f: FA[F, A, B], g: FA[F, C, D]): FA[F, (A, C), (B, D)] = self.split(f, g)
    override def merge[A, B, C](f: FA[F, A, B], g: FA[F, A, C]): FA[F, A, (B, C)] = self.merge(f, g)
  }

  implicit def freeArrowApplicative[F[_, _], C]: Applicative[FA[F, C, ?]] = new Applicative[FA[F, C, ?]] {
    def pure[A](x: A): FA[F, C, A] = FreeArrow.arr(_ => x)
    def ap[A, B](ff: FA[F, C, A => B])(fa: FA[F, C, A]): FA[F, C, B] = (ff &&& fa).rmap { case (f, a) => f(a) }
  }



  final private case class Id[A]() extends FA[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, A] = A.id
  }
  final private case class Arr[A, B](f: A => B) extends FA[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FA[F, A, B], end: FA[F, B, C]) extends FA[F, A, C] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, C] = {
      val la = lazyAnd(fk)
      val eval = for {
        e <- la(begin)
        b <- Eval.later(la(end)).flatten
      } yield e andThen b
      eval.value
    }

    type EvalG[G[_, _], X, Y] = Eval[G[X, Y]]
    def lazyAnd[H[_, _], G[_, _]: Arrow](fk: H ~~> G): FA[H, ?, ?] ~~> EvalG[G, ?, ?] = new (FA[H, ?, ?] ~~> EvalG[G, ?, ?]) {
      def apply[D, E](f: FA[H, D, E]): EvalG[G, D, E] = f match {
        case a: AndThen[H, D, b, E] =>

          for {
            b <- Eval.later(apply(a.begin)).flatten
            e <- apply(a.end)
          } yield b.andThen(e)

        case _ => Eval.now(f.foldMap(fk))
      }
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

    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }

  private def arrowToChoice[F[_, _]]: FA[F, ?, ?] ~~> FreeArrowChoice[F, ?, ?] =
    new (FA[F, ?, ?] ~~> FreeArrowChoice[F, ?, ?]) { self =>
      def apply[A, B](f: FA[F, A, B]): FreeArrowChoice[F, A, B] = f match {
        case _: Id[a]                     => FAC.Id[a]()
        case Arr(f)                      => FAC.Pure(f)
        case l: Lift[F, A, B]             => FAC.Lift(l.fab)
        case a: AndThen[F, A, b, B]       => FAC.AndThen(self(a.begin), self(a.end))
        case m: Merge[f, a, b, c]         => FAC.Merge[f, a, b, c](self(m._first), self(m._second))
        case fst: First[f, a, b, c]       => FAC.First[f, a, b, c](self(fst._first))
        case s: Second[f, a, b, c]        => FAC.Second[f, a, b, c](self(s._second))
        case s: Split[f, a, b, c, d]      => FAC.Split[f, a, b, c, d](self(s._first), self(s._second))
      }
    }
}
