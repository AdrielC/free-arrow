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

  private abstract class ArrowLoopInstance[F[_, _]](A: Arrow[F]) extends ArrowLoop[F] {
    def lift[A, B](f: A => B): F[A, B] = A.lift(f)
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = A.compose(f, g)
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = A.first(fa)
  }
}
