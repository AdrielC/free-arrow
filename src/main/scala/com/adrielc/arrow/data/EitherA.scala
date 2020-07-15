package com.adrielc.arrow
package data

import cats.arrow.Profunctor
import cats.data.Validated
import cats.syntax.either._

final case class EitherA[F[_, _], G[_, _], A, B](run: Either[F[A, B], G[A, B]]) {

  def rmap[C](f: B => C)(implicit F: Profunctor[F], G: Profunctor[G]): EitherA[F, G, A, C] =
    EitherA(run.bimap(F.rmap(_)(f), G.rmap(_)(f)))

  def lmap[C](f: C => A)(implicit F: Profunctor[F], G: Profunctor[G]): EitherA[F, G, C, B] =
    EitherA(run.bimap(F.lmap(_)(f), G.lmap(_)(f)))

  /**
   * Modify the right side context `G` using transformation `f`.
   */
  def mapK[H[_, _]](f: G ~~> H): EitherA[F, H, A, B] =
    EitherA(run.map(f(_)))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: EitherA[G, F, A, B] =
    EitherA(run.swap)

  def toValidated: Validated[F[A, B], G[A, B]] =
    run.toValidated

  def fold[H[_, _]](f: F ~~> H, g: G FunctionA H): H[A, B] =
    run.fold(f.apply, g.apply)
}

object EitherA {

  def leftc[F[_, _], G[_, _], A, B](x: F[A, B]): EitherA[F, G, A, B] =
    EitherA(Left(x))

  def rightc[F[_, _], G[_, _], A, B](x: G[A, B]): EitherA[F, G, A, B] =
    EitherA(Right(x))

  final class EitherBiKLeft[G[_, _]] private[EitherA] {
    def apply[F[_, _], A, B](fab: F[A, B]): EitherA[F, G, A, B] = EitherA(Left(fab))
  }

  final class EitherBiKRight[F[_, _]] private[EitherA] {
    def apply[G[_, _], A, B](gab: G[A, B]): EitherA[F, G, A, B] = EitherA(Right(gab))
  }

  def left[G[_, _]]: EitherBiKLeft[G] = new EitherBiKLeft[G]

  def right[F[_, _]]: EitherBiKRight[F] = new EitherBiKRight[F]
}