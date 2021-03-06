package com.adrielc.quivr.finagle

import com.adrielc.quivr.ArrowChoiceZero
import com.twitter.finagle.Service
import com.twitter.util.Future

trait ServiceInstances {

  implicit val serviceArrow: ArrowChoiceZero[Service] = new ArrowChoiceZero[Service] {

    final def plus[A, B](f: Service[A, B], g: Service[A, B]): Service[A, B] =
      Service.mk(a => f(a) or g(a))

    final def zeroArrow[B, C]: Service[B, C] =
      Service.const(Future.never)

    final def choose[A, B, C, D](f: Service[A, C])(g: Service[B, D]): Service[Either[A, B], Either[C, D]] =
      Service.mk(_.fold(f(_).map(Left(_)), g(_).map(Right(_))))

    final def lift[A, B](f: A => B): Service[A, B] =
      Service.mk(a => Future.value(f(a)))

    final def compose[A, B, C](f: Service[B, C], g: Service[A, B]): Service[A, C] =
      Service.mk(g.andThen(_.flatMap(f)))

    final def first[A, B, C](fa: Service[A, B]): Service[(A, C), (B, C)] =
      Service.mk { case (a, c) => fa(a).map(_ -> c) }

    override def dimap[A, B, C, D](fab: Service[A, B])(f: C => A)(g: B => D): Service[C, D] =
      Service.mk(c => fab.map(f)(c).map(g))

    override final def lmap[A, B, C](fab: Service[A, B])(f: C => A): Service[C, B] = fab.map(f)
  }
}