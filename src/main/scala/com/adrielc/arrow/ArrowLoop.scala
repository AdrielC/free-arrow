package com.adrielc.arrow

import cats.arrow.Arrow
import simulacrum.typeclass
import cats.instances.function.catsStdInstancesForFunction1
import com.adrielc.arrow.ArrowLoop.Loop
import com.adrielc.arrow.data.LazyTuple

@typeclass trait ArrowLoop[~>[_, _]] extends Arrow[~>] {

  def loop[A, B, C](f: Loop[~>, A, B, C]): A ~> B
}

object ArrowLoop {

  type Loop[~>[_, _], A, B, C] = (A :&: C) ~> (B :&: C)

  implicit val functionArrowLoop: ArrowLoop[Function1] = new ArrowLoopInstance(catsStdInstancesForFunction1) {
    def loop[A, B, C](f: Loop[Function1, A, B, C]): A => B = { a =>
      new { val cd: B :&: C = f(LazyTuple(a, cd._2)) }.cd._1
    }
  }

  abstract class ArrowLoopInstance[F[_, _]](A: Arrow[F]) extends ArrowLoop[F] {
    def lift[A, B](f: A => B): F[A, B] = A.lift(f)
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = A.compose(f, g)
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = A.first(fa)
    override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = A.second(fa)
    override def id[A]: F[A, A] = A.id
    override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = A.split(f, g)
    override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = A.merge(f, g)
    override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = A.rmap(fab)(f)
    override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] = A.lmap(fab)(f)
  }
}
