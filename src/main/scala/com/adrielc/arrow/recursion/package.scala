package com.adrielc.arrow

package object recursion {

  type ArAlgebra[A[_[_, _], _, _], F[_, _]]   = A[F, ?, ?] ~~> F
  type ArCoalgebra[A[_[_, _], _, _], F[_, _]] = F ~~> A[F, ?, ?]

  def cataNT[S[_[_, _], _, _], F[_, _]](
    alg: ArAlgebra[S, F]
  )(implicit S: ArFunctor[S]): Fix[S, ?, ?] ~~> F =
    new (Fix[S, ?, ?] ~~> F) { self =>

      def apply[A, B](f: Fix[S, A, B]): F[A, B] =
        alg(S.armap(self)(f.unFix))
    }

  def hyloNT[S[_[_, _], _, _], F[_, _], G[_, _]](
    coAlg: ArCoalgebra[S, F],
    alg: ArAlgebra[S, G]
  )(
    implicit S: ArFunctor[S]
  ): F ~~> G = new (F ~~> G) { self =>

    def apply[A, B](fa: F[A, B]): G[A, B] =
      alg(S.armap(self)(coAlg(fa)))
  }
}
