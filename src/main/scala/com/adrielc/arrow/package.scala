package com.adrielc

import cats.arrow.{Arrow, ArrowChoice}

package object arrow {

  type ~~>[-F[_, _], +G[_, _]] = FunctionP[F, G]

  type ~>>[-F[_, _], +M] = ~~>[F, Const[M]#λ]

  type Const[C] = {
    type λ[A, B] = C
  }

  val LazyTuple = data.LazyTuple
  type :&:[A,B] = LazyTuple.LazyTuple2[A,B]
}

package arrow {

  abstract class ComposedArrowChoice[F[_, _]](A: Arrow[F]) extends ArrowChoice[F] {

    def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]

    def leftImpl[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]]

    def rightImpl[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]]

    def choiceImpl[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C]

    def lift[A, B](f: A => B): F[A, B] = A.lift(f)
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = A.compose(f, g)
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = A.first(fa)
    final override def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] = super.choice(f, g)
    final override def right[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]] = rightImpl(fab)
    final override def left[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]] = leftImpl(fab)
    final override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = A.second(fa)
    final override def id[A]: F[A, A] = A.id
    final override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = A.split(f, g)
    final override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = A.merge(f, g)
    final override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = A.rmap(fab)(f)
    final override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] = A.lmap(fab)(f)
  }

  abstract class ComposedArrowChoicePlus[F[_, _]](A: ArrowChoice[F]) extends ComposedArrowChoice(A) with ArrowChoicePlus[F] {

    def zeroArrow[B, C]: F[B, C]

    def plus[A, B](f: F[A, B], g: F[A, B]): F[A, B]

    final def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]] = A.choose(f)(g)
    final override def leftImpl[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]] = A.left(fab)
    final override def rightImpl[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]] = A.right(fab)
    final override def choiceImpl[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] = A.choice(f, g)
  }
}
