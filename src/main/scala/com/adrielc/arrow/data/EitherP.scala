package com.adrielc.arrow
package data

import cats.arrow.Profunctor
import cats.data.Validated
import cats.syntax.either._

final case class EitherP[F[_, _], G[_, _], A, B](run: Either[F[A, B], G[A, B]]) {

  def rmap[C](f: B => C)(implicit F: Profunctor[F], G: Profunctor[G]): EitherP[F, G, A, C] =
    EitherP(run.bimap(F.rmap(_)(f), G.rmap(_)(f)))

  def lmap[C](f: C => A)(implicit F: Profunctor[F], G: Profunctor[G]): EitherP[F, G, C, B] =
    EitherP(run.bimap(F.lmap(_)(f), G.lmap(_)(f)))

  /**
   * Modify the right side context `G` using transformation `f`.
   */
  def mapK[H[_, _]](f: G FunctionP H): EitherP[F, H, A, B] =
    EitherP(run.map(f(_)))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: EitherP[G, F, A, B] =
    EitherP(run.swap)

  def toValidated: Validated[F[A, B], G[A, B]] =
    run.toValidated

  def fold[H[_, _]](f: F FunctionP H, g: G FunctionP H): H[A, B] =
    run.fold(f.apply, g.apply)
}

object EitherP {

  def leftc[F[_, _], G[_, _], A, B](x: F[A, B]): EitherP[F, G, A, B] =
    EitherP(Left(x))

  def rightc[F[_, _], G[_, _], A, B](x: G[A, B]): EitherP[F, G, A, B] =
    EitherP(Right(x))

  final class EitherBiKLeft[G[_, _]] private[EitherP] {
    def apply[F[_, _], A, B](fab: F[A, B]): EitherP[F, G, A, B] = EitherP(Left(fab))
  }

  final class EitherBiKRight[F[_, _]] private[EitherP] {
    def apply[G[_, _], A, B](gab: G[A, B]): EitherP[F, G, A, B] = EitherP(Right(gab))
  }

  def left[G[_, _]]: EitherBiKLeft[G] = new EitherBiKLeft[G]

  def right[F[_, _]]: EitherBiKRight[F] = new EitherBiKRight[F]
}