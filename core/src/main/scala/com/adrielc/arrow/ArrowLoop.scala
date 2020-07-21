package com.adrielc.arrow

import cats.arrow.Arrow
import cats.instances.function.catsStdInstancesForFunction1
import com.adrielc.arrow.data.{:&:, lazyTuple}

trait ArrowLoop[~>[_, _]] extends Arrow[~>] {

  def loop[A, B, C](f: (A :&: C) ~> (B :&: C)): A ~> B
}

object ArrowLoop {

  def apply[F[_, _]](implicit A: ArrowLoop[F]): ArrowLoop[F] = A

  implicit val functionArrowLoop: ArrowLoop[Function1] = new ArrowLoopInstance[Function1] {

    val algebra: Arrow[Function1] = catsStdInstancesForFunction1

    def loop[A, B, C](f: (A :&: C) => (B :&: C)): A => B = { a =>
      new { val cd: B :&: C = f(lazyTuple(a, cd._2)) }.cd._1.value
    }
  }
}

private[arrow] trait ArrowLoopInstance[F[_, _]] extends ArrowLoop[F] {

  def algebra: Arrow[F]

  def lift[A, B](f: A => B): F[A, B] = algebra.lift(f)
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = algebra.compose(f, g)
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = algebra.first(fa)
  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = algebra.second(fa)
  override def id[A]: F[A, A] = algebra.id
  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = algebra.split(f, g)
  override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = algebra.merge(f, g)
  override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = algebra.rmap(fab)(f)
  override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] = algebra.lmap(fab)(f)
}
