package com.adrielc.quivr.metrics.data

import cats.{Monad, Monoid, MonoidK, Semigroup}
import cats.implicits._

case class AccumWriter[L, A](l: L, run: Option[A]) {

  def log(b: L)(implicit S: Semigroup[L]): AccumWriter[L, A] =
    AccumWriter(b |+| l, run)

  def flatMap[B](f: A => AccumWriter[L, B])(implicit S: Semigroup[L]): AccumWriter[L, B] =
    run.map(f(_).log(l)).getOrElse(AccumWriter(l, None))
}
object AccumWriter {

  implicit def accumWriterMonoidKFlatMap[L: Monoid]: MonoidK[AccumWriter[L, *]] with Monad[AccumWriter[L, *]] =
    new MonoidK[AccumWriter[L, *]] with Monad[AccumWriter[L, *]] {

      override def combineK[A](x: AccumWriter[L, A], y: AccumWriter[L, A]): AccumWriter[L, A] =
        AccumWriter(x.l |+| y.l, x.run.orElse(y.run))

      override def empty[A]: AccumWriter[L, A] =
        AccumWriter(Monoid.empty, None)

      override def flatMap[A, B](fa: AccumWriter[L, A])(f: A => AccumWriter[L, B]): AccumWriter[L, B] =
        fa.flatMap(f)

      override def pure[A](x: A): AccumWriter[L, A] =
        AccumWriter(Monoid.empty, Some(x))

      override def tailRecM[A, B](a: A)(f: A => AccumWriter[L, Either[A, B]]): AccumWriter[L, B] = {

        @annotation.tailrec
        def loop(x: L, aa: A): AccumWriter[L, B] =
          f(aa) match {
            case AccumWriter(nextL, Some(Left(nextA))) => loop(x |+| nextL, nextA)
            case AccumWriter(nextL, Some(Right(b)))    => AccumWriter(x |+| nextL, Some(b))
            case AccumWriter(nextL, None)              => AccumWriter(x |+| nextL, None)
          }

        f(a) match {
          case AccumWriter(nextL, None)              => AccumWriter(nextL, None)
          case AccumWriter(nextL, Some(Right(b)))    => AccumWriter(nextL, Some(b))
          case AccumWriter(nextL, Some(Left(nextA))) => loop(nextL, nextA)
        }
      }
    }
}
