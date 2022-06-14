package com.adrielc.quivr

import cats.instances.function.catsStdInstancesForFunction1
import com.adrielc.quivr.data.{:&:, lazyTuple}

trait ArrowLoop[~>[_, _]] extends Arrow[~>] {

  def loop[A, B, C](f: (A :&: C) ~> (B :&: C)): A ~> B
}

object ArrowLoop {

  def apply[F[_, _]](implicit A: ArrowLoop[F]): ArrowLoop[F] = A

  implicit val functionArrowLoop: ArrowLoop[Function1] = new instances.ComposedArrowInstance[Function1] with ArrowLoop[Function1] {

    implicit val A: cats.arrow.Arrow[Function1] = catsStdInstancesForFunction1

    def loop[A, B, C](f: (A :&: C) => (B :&: C)): A => B = { a =>
      new { val cd: B :&: C = f(lazyTuple(a, cd._2)) }.cd._1.value
    }
  }
}