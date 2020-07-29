package com.adrielc.arrow.data

import cats.arrow.Profunctor
import cats.data.Validated
import cats.syntax.either._

final case class BiEitherK[+F[_, _], +G[_, _], A, B](run: Either[F[A, B], G[A, B]]) {

  def rmap[FF[a, b] >: F[a, b], GG[a, b] >: G[a, b], C](f: B => C)(implicit F: Profunctor[FF], G: Profunctor[GG]): BiEitherK[FF, GG, A, C] =
    BiEitherK(run.bimap(F.rmap(_)(f), G.rmap(_)(f)))

  def lmap[FF[a, b] >: F[a, b], GG[a, b] >: G[a, b], C](f: C => A)(implicit F: Profunctor[FF], G: Profunctor[GG]): BiEitherK[FF, GG, C, B] =
    BiEitherK(run.bimap(F.lmap(_)(f), G.lmap(_)(f)))

  /**
   * Modify the right side context `G` using transformation `f`.
   */
  def mapK[H[_, _]](f: G ~~> H): BiEitherK[F, H, A, B] =
    BiEitherK(run.map(f(_)))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: BiEitherK[G, F, A, B] =
    BiEitherK(run.swap)

  def toValidated: Validated[F[A, B], G[A, B]] =
    run.toValidated

  def fold[H[_, _]](f: F ~~> H, g: G ~~> H): H[A, B] =
    run.fold(f.apply, g.apply)
}

object BiEitherK {

  def leftc[F[_, _], G[_, _], A, B](fab: F[A, B]): BiEitherK[F, G, A, B] = BiEitherK(Left(fab))

  def rightc[F[_, _], G[_, _], A, B](gab: G[A, B]): BiEitherK[F, G, A, B] = BiEitherK(Right(gab))

  final class EitherBiKLeft[G[_, _]] private[BiEitherK] {
    def apply[F[_, _], A, B](fab: F[A, B]): BiEitherK[F, G, A, B] = leftc(fab)
  }

  final class EitherBiKRight[F[_, _]] private[BiEitherK] {
    def apply[G[_, _], A, B](gab: G[A, B]): BiEitherK[F, G, A, B] = rightc(gab)
  }

  def left[G[_, _]]: EitherBiKLeft[G] = new EitherBiKLeft[G]

  def right[F[_, _]]: EitherBiKRight[F] = new EitherBiKRight[F]

  /** [[BiFunctionK]] variant of [[leftc]] */
  def leftK[F[_, _], G[_, _]]:  F ~~> BiEitherK[F, G, ?, ?] = BiFunctionK.lift(leftc)

  /** [[BiFunctionK]] variant of [[rightc]] */
  def rightK[F[_, _], G[_, _]]:  G ~~> BiEitherK[F, G, ?, ?] = BiFunctionK.lift(rightc)
}