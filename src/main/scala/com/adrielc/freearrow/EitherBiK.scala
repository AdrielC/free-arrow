package com.adrielc.freearrow

import cats.arrow.Profunctor
import cats.data.Validated
import cats.syntax.either._

final case class EitherBiK[F[_, _], G[_, _], A, B](run: Either[F[A, B], G[A, B]]) {

  def rmap[C](f: B => C)(implicit F: Profunctor[F], G: Profunctor[G]): EitherBiK[F, G, A, C] =
    EitherBiK(run.bimap(F.rmap(_)(f), G.rmap(_)(f)))

  def lmap[C](f: C => A)(implicit F: Profunctor[F], G: Profunctor[G]): EitherBiK[F, G, C, B] =
    EitherBiK(run.bimap(F.lmap(_)(f), G.lmap(_)(f)))

  /**
   * Modify the right side context `G` using transformation `f`.
   */
  def mapK[H[_, _]](f: G ~~> H): EitherBiK[F, H, A, B] =
    EitherBiK(run.map(f(_)))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: EitherBiK[G, F, A, B] =
    EitherBiK(run.swap)

  def toValidated: Validated[F[A, B], G[A, B]] =
    run.toValidated

  def fold[H[_, _]](f: F ~~> H, g: G ~~> H): H[A, B] =
    run.fold(f.apply, g.apply)
}

object EitherBiK {

  def leftc[F[_, _], G[_, _], A, B](x: F[A, B]): EitherBiK[F, G, A, B] =
    EitherBiK(Left(x))

  def rightc[F[_, _], G[_, _], A, B](x: G[A, B]): EitherBiK[F, G, A, B] =
    EitherBiK(Right(x))

  final class EitherBiKLeft[G[_, _]] private[EitherBiK] {
    def apply[F[_, _], A, B](fab: F[A, B]): EitherBiK[F, G, A, B] = EitherBiK(Left(fab))
  }

  final class EitherBiKRight[F[_, _]] private[EitherBiK] {
    def apply[G[_, _], A, B](gab: G[A, B]): EitherBiK[F, G, A, B] = EitherBiK(Right(gab))
  }

  def left[G[_, _]]: EitherBiKLeft[G] = new EitherBiKLeft[G]

  def right[F[_, _]]: EitherBiKRight[F] = new EitherBiKRight[F]
}