package com.adrielc.quivr

import cats.Applicative
import cats.arrow.Profunctor
import cats.data.{Kleisli, Reader}
import com.adrielc.quivr.data._
import cats.instances.list._

/**
 *
 * BiNatural/ExtraNatural Transformation
 *
 * A function universally quantified over two parameters.
 * Like [[cats.arrow.FunctionK]] but for Profunctors
 *
 */
trait BiFunctionK[-F[_, _], +G[_, _]] extends Serializable {
  self =>

  def apply[A, B](fab: F[A, B]): G[A, B]

  def compose[H[_, _]](f: H ~~> F): H ~~> G =
    new (H ~~> G) {
      def apply[A, B](eab: H[A, B]): G[A, B] = self(f(eab))
    }

  def andThen[H[_, _]](g: G ~~> H): F ~~> H = g.compose(self)


  def and[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](other: FF ~~> H): FF ~~> BiTuple2K[GG, H, *, *] =
    new (FF ~~> BiTuple2K[GG, H, *, *]) {
      def apply[A, B](f: FF[A, B]): BiTuple2K[GG, H, A, B] = BiTuple2K(self(f), other(f))
    }

  def or[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](h: H ~~> GG): BiEitherK[FF, H, *, *] ~~> GG =
    new (BiEitherK[FF, H, *, *] ~~> GG) {
      override def apply[A, B](f: BiEitherK[FF, H, A, B]): GG[A, B] = f.run.fold(self(_), h(_))
    }

  def pureOut[M[_], GG[a, b] >: G[a, b]](implicit P: Profunctor[GG], A: Applicative[M]): F ~~> λ[(α, β) => GG[α, M[β]]] =
    new (F ~~> λ[(α, β) => GG[α, M[β]]]) {
      def apply[A, B](fab: F[A, B]): GG[A, M[B]] = P.rmap(self(fab))(A.pure)
    }

  def pureOutK[M[_], GG[a, b] >: G[a, b]](implicit A: Applicative[M]): F ~~> λ[(α, β) => M[GG[α, β]]] =
    new (F ~~> λ[(α, β) => M[GG[α, β]]]) {
      def apply[A, B](fab: F[A, B]): M[GG[A, B]] = A.pure(self(fab))
    }
}

object BiFunctionK {

  def id[F[_, _]]: F ~~> F = new (F ~~> F) { def apply[A, B](f: F[A, B]): F[A, B] = f }

  def apply[F[_, _], G[_, _]](implicit B: F ~~> G): F ~~> G = B


  implicit class ToFunctionOps[F[_, _]](private val fk: Pure[F]) extends AnyVal {

    def kleisli[M[_]: Applicative]: F ~~> Kleisli[M, *, *] = fk.andThen(BiFunctionK.kleisli[M])
  }

  def kleisli[F[_]](implicit A: Applicative[F]): Function1 ~~> Kleisli[F, *, *] =
    new (Function1 ~~> Kleisli[F, *, *]) { def apply[A, B](fab: A => B): Kleisli[F, A, B] = Reader(fab).lift[F] }

  def collect[F[_, _]]: F ~>| List[F[_, _]] = BiFunctionK.id[F].pureOutK[List, F]

  def pure[M[_]: Applicative] = new PureOps[M]

  final class PureOps[M[_]] private[BiFunctionK] (implicit A: Applicative[M]) {
    def self[F[_, _]]: F ~~> λ[(α, β) => M[F[α, β]]] = new (F ~~> λ[(α, β) => M[F[α, β]]]) {
      def apply[A, B](fab: F[A, B]): M[F[A, B]] = A.pure(fab)
    }
    def out[F[_, _]](implicit P: Profunctor[F]): F ~~> λ[(α, β) => F[α, M[β]]] = new (F ~~> λ[(α, β) => F[α, M[β]]]) {
      def apply[A, B](fab: F[A, B]): F[A, M[B]] = P.rmap(fab)(A.pure)
    }
  }

  /**
   * Lifts function `f` of `F[A, B] => G[A, B]` into a `BiFunctionK[F, G]`.
   *
   * {{{
   *   def toMap[A, B](kv: (A, B)): Map[A, B] = Map(kv)
   *   val lifted: BiFunctionK[Tuple2, Map] = BiFunctionK.lift(toMap)
   * }}}
   *
   * Note: This method has a macro implementation that returns a new
   * `BiFunctionK` instance as follows:
   *
   * {{{
   *   new BiFunctionK[F[_, _], G[_, _]] {
   *     def apply[A, B](fa: F[A, B]): G[A, B] = f(fa)
   *   }
   * }}}
   *
   * Additionally, the type parameters on `f` must not be specified.
   */
  def lift[F[_, _], G[_, _]](f: (F[α, β] => G[α, β]) forSome { type α; type β }): BiFunctionK[F, G] =
  macro BiFunctionKMacros.lift[BiFunctionK, F, G]
}