package com.adrielc.quivr

import cats.{Applicative, ~>}
import cats.arrow.Profunctor
import cats.data.{Kleisli, Reader}
import com.adrielc.quivr.data._
import cats.instances.{function, list}
import function._
import list._

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
}

object BiFunctionK {

  def apply[F[_, _], G[_, _]](implicit B: F ~~> G): F ~~> G = B

  def id[F[_, _]]: F ~~> F = new (F ~~> F) { def apply[A, B](f: F[A, B]): F[A, B] = f }

  def pure[M[_]] = new PureOps[M]

  final class PureOps[M[_]] private[BiFunctionK] {

    def lift[F[_, _]](implicit A: Applicative[M]): F ~~> λ[(α, β) => M[F[α, β]]] =
      new (F ~~> λ[(α, β) => M[F[α, β]]]) { def apply[A, B](fab: F[A, B]): M[F[A, B]] = A.pure(fab) }

    def lower[F[_, _]](implicit P: Profunctor[F], A: Applicative[M]): F ~~> λ[(α, β) => F[α, M[β]]] =
      new (F ~~> λ[(α, β) => F[α, M[β]]]) { def apply[A, B](fab: F[A, B]): F[A, M[B]] = P.rmap(fab)(A.pure) }
  }
  implicit class BiFunctionKKleisliOp[F[_, _], G[_]](private val fK: F ~~> Kleisli[G, *, *]) extends AnyVal {

    def mapK[H[_]](f: G ~> H): F ~~> Kleisli[H, *, *] = new (F ~~> Kleisli[H, *, *]) {
      override def apply[A, B](fab: F[A, B]): Kleisli[H, A, B] = fK(fab).mapK(f)
    }
  }
  implicit class BiFunctionKOp[F[_, _], G[_, _]](private val fK: F ~~> G) extends AnyVal {

    def and[H[_, _]](other: F ~~> H): F ~~> BiTuple2K[G, H, *, *] =
      new (F ~~> BiTuple2K[G, H, *, *]) { def apply[A, B](f: F[A, B]): BiTuple2K[G, H, A, B] = BiTuple2K(fK(f), other(f)) }

    def or[H[_, _]](h: H ~~> G): BiEitherK[F, H, *, *] ~~> G =
      new (BiEitherK[F, H, *, *] ~~> G) {
        override def apply[A, B](f: BiEitherK[F, H, A, B]): G[A, B] = f.run.fold(fK(_), h(_))
      }
  }
  implicit class BiFunctionKPureKOp[F[_, _], G[_, _], M[_]](private val fK: F ~~> λ[(α, β) => M[G[α, β]]]) extends AnyVal {

    def mapK[N[_]](f: M ~> N): F ~~> λ[(α, β) => N[G[α, β]]] = new (F ~~> λ[(α, β) => N[G[α, β]]]) {
      override def apply[A, B](fab: F[A, B]): N[G[A, B]] = f(fK(fab))
    }
  }
  implicit class BiFunctionKPureOp[F[_, _], G[_, _], M[_]](private val fK: F ~~> λ[(α, β) => G[α, M[β]]]) extends AnyVal {

    def rmapK[N[_]](f: M ~> N)(implicit P: Profunctor[G]): F ~~> λ[(α, β) => G[α, N[β]]] = new (F ~~> λ[(α, β) => G[α, N[β]]]) {
      override def apply[A, B](fab: F[A, B]): G[A, N[B]] = P.rmap(fK(fab))(f(_))
    }
  }
  implicit class BiFunctionKConstOp[F[_, _], M](private val fK: F ~~> λ[(α, β) => M]) extends AnyVal {

    def const: F ~~> BiConst[M, *, *] = BiConst.liftK(fK)

    def rmap[N](f: M => N): F ~>| N = new (F ~~> λ[(α, β) => N]) {
      override def apply[A, B](fab: F[A, B]): N = f(fK(fab))
    }
  }

  implicit class ToFunctionOps[F[_, _]](private val fk: Pure[F]) extends AnyVal {

    def kleisli[M[_]: Applicative]: F ~~> Kleisli[M, *, *] = fk.andThen(pure[M].lower[Function1].andThen(functionToKleisli))
  }

  def functionToKleisli[M[_]]: λ[(α, β) => α => M[β]] ~~> Kleisli[M, *, *] =
    new (λ[(α, β) => α => M[β]] ~~> Kleisli[M, *, *]) { def apply[A, B](fab: A => M[B]): Kleisli[M, A, B] = Kleisli(fab) }

  val functionToReader: Function1 ~~> Reader =
    BiFunctionK.lift(Reader.apply)

  def collect[F[_, _]]: F ~>| List[F[_, _]] = BiFunctionK.pure[List].lift[F]

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