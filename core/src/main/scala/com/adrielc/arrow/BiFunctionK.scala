package com.adrielc.arrow

import cats.Applicative
import cats.data.Kleisli
import com.adrielc.arrow.data.{BiEitherK, BiTuple2K}

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


  def and[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](other: FF ~~> H): FF ~~> BiTuple2K[GG, H, ?, ?] =
    new (FF ~~> BiTuple2K[GG, H, ?, ?]) {
      def apply[A, B](f: FF[A, B]): BiTuple2K[GG, H, A, B] = BiTuple2K(self(f), other(f))
    }

  def or[H[_, _], FF[a, b] <: F[a, b], GG[a, b] >: G[a, b]](h: H ~~> GG): BiEitherK[FF, H, ?, ?] ~~> GG =
    new (BiEitherK[FF, H, ?, ?] ~~> GG) {
      override def apply[A, B](f: BiEitherK[FF, H, A, B]): GG[A, B] = f.run.fold(self(_), h(_))
    }
}

object BiFunctionK {

  def id[F[_, _]]: F ~~> F = new (F ~~> F) { def apply[A, B](f: F[A, B]): F[A, B] = f }

  def apply[F[_, _], G[_, _]](implicit B: F ~~> G): F ~~> G = B


  implicit class PureOps[F[_, _]](private val fk: Pure[F]) extends AnyVal {

    def kleisli[M[_]: Applicative]: F ~~> Kleisli[M, ?, ?] = fk.andThen(BiFunctionK.kleisli)
  }

  def kleisli[F[_]](implicit A: Applicative[F]): Function1 ~~> Kleisli[F, ?, ?] =
    new (Function1 ~~> Kleisli[F, ?, ?]) { def apply[A, B](fab: A => B): Kleisli[F, A, B] = Kleisli(a => A.pure(fab(a))) }

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