package com.adrielc.arrow.data

import com.adrielc.arrow.~~>

trait FunctionA[-F[_, _], +G[_, _]] extends Serializable {
  self =>

  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[H[_, _]](f: H ~~> F): H ~~> G =
    new (H ~~> G) {
      def apply[A, B](eab: H[A, B]): G[A, B] = self(f(eab))
    }

  def andThen[H[_, _]](g: G ~~> H): F ~~> H = g.compose(self)

  def and[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](other: FF ~~> H): FF ~~> Tuple2A[GG, H, ?, ?] =
    new (FF ~~> Tuple2A[GG, H, ?, ?]) {
      def apply[A, B](f: FF[A, B]): Tuple2A[GG, H, A, B] = Tuple2A(self(f), other(f))
    }

  def or[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](h: H ~~> GG): EitherA[FF, H, ?, ?] ~~> GG =
    new (EitherA[FF, H, ?, ?] ~~> GG) {
      override def apply[A, B](f: EitherA[FF, H, A, B]): GG[A, B] = f.run.fold(self(_), h(_))
    }
}

object FunctionA {

  def id[F[_, _]]: F ~~> F =
    new (F ~~> F) {
      def apply[A, B](f: F[A, B]): F[A, B] = f
    }

  def apply[F[_, _], G[_, _]](implicit B: F ~~> G): F ~~> G = B
}