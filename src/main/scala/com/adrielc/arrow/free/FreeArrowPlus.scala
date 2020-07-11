package com.adrielc.arrow.free

import cats.Eval
import com.adrielc.arrow.{ArrowPlus, ~~>}
import com.adrielc.arrow.free.methods.FreeArrowPlusLike
import cats.implicits._

sealed abstract class FreeArrowPlus[+F[_, _], A, B] extends FreeArrowPlusLike[FreeArrowPlus, ArrowPlus, F, A, B]

object FreeArrowPlus { self =>

  @inline final def id[A]: FAP[Nothing, A, A] = Id()

  /** Lift a pure function into [[FreeArrow]]. Can be composed with any context */
  @inline final def arr[A, B](f: A => B): FAP[Nothing, A, B] = Arr(cats.data.AndThen(f))

  /** Lift an algebra [[F]] into [[FreeArrow]] */
  @inline final def lift[F[_, _], A, B](fab: F[A, B]): FAP[F, A, B] = Lift(fab)

  @inline final def zero[A, B]: FAP[Nothing, A, B] = Zero()

  @inline final def andThen[F[a, b], A, B, C](first: FAP[F, A, B], next: FAP[F, B, C]): FAP[F, A, C] = (first, next) match {
    case (Arr(f), Arr(g)) => Arr(f andThen g)
    case (Arr(f), AndThen(Arr(g), e)) => AndThen(Arr(f andThen g), e)
    case (AndThen(a, Arr(f)), Arr(g)) => AndThen(a, Arr(f andThen g))
    case (AndThen(a, Arr(f)), AndThen(Arr(g), e)) => AndThen(a, AndThen(Arr(f andThen g), e))
    case _ => AndThen(first, next)
  }

  @inline final def merge[F[_, _], A, B, C](fab: FAP[F, A, B], fac: FAP[F, A, C]): FAP[F, A, (B, C)] = (fab, fac) match {
    case (Arr(f), Arr(g)) => arr(a => (f(a), g(a)))
    case _ => Merge(fab, fac)
  }

  @inline final def rmap[F[_, _], A, B, C](fab: FAP[F, A, B])(f: B => C): FAP[F, A, C] = andThen(fab, arr(f))
  @inline final def lmap[F[_, _], A, B, C](fab: FAP[F, A, B])(f: C => A): FAP[F, C, B] = andThen(arr(f), fab)
  @inline final def first[F[_, _], A, B, C](fab: FAP[F, A, B]): FAP[F, (A, C), (B, C)] = First(fab)
  @inline final def second[F[_, _], A, B, C](fab: FAP[F, A, B]): FAP[F, (C, A), (C, B)] = Second(fab)
  @inline final def split[F[_, _], A, B, C, D](fab: FAP[F, A, B], fcd: FAP[F, C, D]): FAP[F, (A, C), (B, D)] = Split(fab , fcd)
  @inline final def plus[F[_, _], A, B](f: FAP[F, A, B], g: FAP[F, A, B]): FAP[F, A, B] = Plus(f, g)


  final implicit def freeArrArrowPlus[F[_, _]]: ArrowPlus[FAP[F, ?, ?]] = new ArrowPlus[FAP[F, ?, ?]] {
    def plus[A, B](f: FAP[F, A, B], g: FAP[F, A, B]): FAP[F, A, B] = FAP.plus(f, g)
    def zeroArrow[B, C]: FAP[F, B, C] = ???
    def lift[A, B](f: A => B): FAP[F, A, B] = FAP.arr(f)
    def compose[A, B, C](f: FAP[F, B, C], g: FAP[F, A, B]): FAP[F, A, C] = FAP.andThen(g, f)
    def first[A, B, C](fa: FAP[F, A, B]): FAP[F, (A, C), (B, C)] = FAP.first(fa)
    override def second[A, B, C](fa: FAP[F, A, B]): FAP[F, (C, A), (C, B)] = FAP.second(fa)
    override def split[A, B, C, D](f: FAP[F, A, B], g: FAP[F, C, D]): FAP[F, (A, C), (B, D)] = FAP.split(f, g)
    override def merge[A, B, C](f: FAP[F, A, B], g: FAP[F, A, C]): FAP[F, A, (B, C)] = FAP.merge(f, g)
    override def rmap[A, B, C](fab: FAP[F, A, B])(f: B => C): FAP[F, A, C] = andThen(fab, lift(f))
    override def lmap[A, B, C](fab: FAP[F, A, B])(f: C => A): FAP[F, C, B] = compose(fab, lift(f))
  }

  final private case class Id[A]() extends FAP[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowPlus[G]): G[A, A] = A.id
  }
  final private case class Arr[A, B](f: cats.data.AndThen[A, B]) extends FAP[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowPlus[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FAP[F, A, B], end: FAP[F, B, C]) extends FAP[F, A, C] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[A, C] = {
      type EvalG[X, Y] = Eval[G[X, Y]]
      lazy val lazyAnd = new (FAP[F, ?, ?] ~~> EvalG) {
        def apply[D, E](f: FAP[F, D, E]): EvalG[D, E] = f match {
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
  final private case class Lift[F[_, _], A, B](fab: F[A, B]) extends FAP[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[A, B] = fk(fab)
  }
  final private case class Merge[F[_, _], A, B, C](_first: FAP[F, A, B], _second: FAP[F, A, C]) extends FAP[F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[A, (B, C)] = A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class Split[F[_, _], A, B, C, D](_first: FAP[F, A, B], _second: FAP[F, C, D]) extends FAP[F, (A, C), (B, D)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[(A, C), (B, D)] = A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class First[F[_, _], A, B, C](_first: FAP[F, A, B]) extends FAP[F, (A, C), (B, C)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[(A, C), (B, C)] = A.first(_first.foldMap(fk))
  }
  final private case class Second[F[_, _], A, B, C](_second: FAP[F, A, B]) extends FAP[F, (C, A), (C, B)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }
  final private case class Zero[A, B]() extends FAP[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowPlus[G]): G[A, B] = A.zeroArrow
  }
  final private case class Plus[F[_, _], A, B](f: FAP[F, A, B], g: FAP[F, A, B]) extends FreeArrowPlus[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowPlus[G]): G[A, B] = A.plus(f.foldMap(fk), g.foldMap(fk))
  }
}
