package com.adrielc.arrow

import com.adrielc.arrow.data.{EitherP, Tuple2P}

/**
 *
 * ExtraNatural/BiNatural Transformation
 *
 * A function universally quantified over two parameters.
 * Like [[cats.arrow.FunctionK]] but for Profunctors
 *
 */
trait FunctionP[-F[_, _], +G[_, _]] extends Serializable {
  self =>

  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[H[_, _]](f: H ~~> F): H ~~> G =
    new (H ~~> G) {
      def apply[A, B](eab: H[A, B]): G[A, B] = self(f(eab))
    }

  def andThen[H[_, _]](g: G ~~> H): F ~~> H = g.compose(self)

  def and[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](other: FF ~~> H): FF ~~> Tuple2P[GG, H, ?, ?] =
    new (FF ~~> Tuple2P[GG, H, ?, ?]) {
      def apply[A, B](f: FF[A, B]): Tuple2P[GG, H, A, B] = Tuple2P(self(f), other(f))
    }

  def or[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](h: H ~~> GG): EitherP[FF, H, ?, ?] ~~> GG =
    new (EitherP[FF, H, ?, ?] ~~> GG) {
      override def apply[A, B](f: EitherP[FF, H, A, B]): GG[A, B] = f.run.fold(self(_), h(_))
    }
}

object FunctionP {

  def id[F[_, _]]: F ~~> F =
    new (F ~~> F) {
      def apply[A, B](f: F[A, B]): F[A, B] = f
    }

  def apply[F[_, _], G[_, _]](implicit B: F ~~> G): F ~~> G = B
}