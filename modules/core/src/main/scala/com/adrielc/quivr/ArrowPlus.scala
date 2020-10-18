package com.adrielc.quivr

import cats.data.Kleisli
import cats.{Alternative, Monad, Monoid, MonoidK}
import simulacrum.{op, typeclass}
import cats.data.{Kleisli, IndexedReaderWriterStateT => IRWST}

@typeclass trait ArrowPlus[~>[_, _]] extends ArrowZero[~>] {

  @op("<+>", alias = true)
  def plus[A, B](f: A ~> B, g: A ~> B): A ~> B
}

object ArrowPlus {

  implicit def kleisliArrowPlus[M[_]: Monad : MonoidK]: ArrowPlus[Kleisli[M, *, *]] = ArrowChoicePlus.arrowChoicePlusForKleisli

  def arrowPlusForIRWST[E, L, F[_], T](implicit A: Alternative[F], M: Monad[F], MT: Monoid[T], ML: Monoid[L]): ArrowPlus[IRWST[F, E, L, *, *, T]] = new ArrowPlus[IRWST[F, E, L, *, *, T]] {
    import cats.data.IndexedReaderWriterStateT.catsDataStrongForIRWST
    import cats.implicits._

    override def plus[A, B](f: IRWST[F, E, L, A, B, T], g: IRWST[F, E, L, A, B, T]): IRWST[F, E, L, A, B, T] =
      IRWST.apply((e: E, a: A) => f.run(e, a) <+> g.run(e, a))(A)

    override def zeroArrow[B, C]: IRWST[F, E, L, B, C, T] =
      IRWST((_: E, _: B) => A.empty[(L, C, T)])(A)

    override def lift[A, B](f: A => B): IRWST[F, E, L, A, B, T] =
      IRWST.modify[F, E, L, A, B](f)(A, ML).map(_ => Monoid[T].empty)(A)

    override def compose[A, B, C](f: IRWST[F, E, L, B, C, T], g: IRWST[F, E, L, A, B, T]): IRWST[F, E, L, A, C, T] =
      IRWST { (e: E, a: A) =>
        g.run(e, a).flatMap { case (l, b, t) =>
          A.map(f.run(e, b)) { case (l2, c, t2) =>
            (l |+| l2, c, t |+| t2)
          }
        }
      }(A)

    override def first[A, B, C](fa: IRWST[F, E, L, A, B, T]): IRWST[F, E, L, (A, C), (B, C), T] =
      catsDataStrongForIRWST[F, E, L, T].first(fa)
  }
}