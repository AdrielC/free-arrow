package com.adrielc.quivr
package data

import cats.arrow.Profunctor
import cats.data.Validated
import cats.syntax.either._
import com.adrielc.quivr.BiFunctionK

final case class BiEitherK[+F[_, _], +G[_, _], A, B](run: Either[F[A, B], G[A, B]]) {

  def rmap[FF[a, b] >: F[a, b], GG[a, b] >: G[a, b], C](f: B => C)(implicit F: Profunctor[FF], G: Profunctor[GG]): BiEitherK[FF, GG, A, C] =
    BiEitherK(run.bimap(F.rmap(_)(f), G.rmap(_)(f)))

  def lmap[FF[a, b] >: F[a, b], GG[a, b] >: G[a, b], C](f: C => A)(implicit F: Profunctor[FF], G: Profunctor[GG]): BiEitherK[FF, GG, C, B] =
    BiEitherK(run.bimap(F.lmap(_)(f), G.lmap(_)(f)))

  /**
   * Modify the right side context `G` using transformation `f`.
   */
  def mapK[H[_, _]](f: G ~~> H): (F or H) # T[A, B] =
    BiEitherK(run.map(f(_)))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: (G or F) # T[A, B] =
    BiEitherK(run.swap)

  def toValidated: Validated[F[A, B], G[A, B]] =
    run.toValidated

  def fold[H[_, _]](f: F ~~> H, g: G ~~> H): H[A, B] =
    run.fold(f.apply, g.apply)
}

object BiEitherK {

  def leftc[F[_, _], G[_, _], A, B](fab: F[A, B]): (F or G) #T[A, B] = BiEitherK(Left(fab))

  def rightc[F[_, _], G[_, _], A, B](gab: G[A, B]): (F or G) #T[A, B] = BiEitherK(Right(gab))

  final class EitherBiKLeft[G[_, _]] private[BiEitherK] {
    def apply[F[_, _], A, B](fab: F[A, B]): (F or G) #T[A, B] = leftc(fab)
  }

  final class EitherBiKRight[F[_, _]] private[BiEitherK] {
    def apply[G[_, _], A, B](gab: G[A, B]): (F or G) #T[A, B] = rightc(gab)
  }

  def left[G[_, _]]: EitherBiKLeft[G] = new EitherBiKLeft[G]

  def right[F[_, _]]: EitherBiKRight[F] = new EitherBiKRight[F]

  /** [[BiFunctionK]] variant of [[leftc]] */
  def leftK[F[_, _], G[_, _]]: F ~~> (F or G)#T = BiFunctionK.lift[F, BiEitherK[F, G, *, *]](leftc)

  /** [[BiFunctionK]] variant of [[rightc]] */
  def rightK[F[_, _], G[_, _]]: G ~~> (F or G)#T = BiFunctionK.lift[G, BiEitherK[F, G, *, *]](rightc)
}