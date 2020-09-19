package com.adrielc.quivr

import com.adrielc.quivr.data.{~>|, ~~>}

package object recursion {

  type Alg[A[_[_, _], _, _], F[_, _]]   = A[F, ?, ?] ~~> F

  type Coalg[A[_[_, _], _, _], F[_, _]] = F ~~> A[F, ?, ?]

  type CoalgEnv[A[_[_, _], _, _], F[_, _], M] = λ[(α, β) => (M, F[α, β])] ~~> A[F, ?, ?]

  type Analyze[A[_[_, _], _, _], F[_, _], M] = A[F, ?, ?] ~>| M

  type CofreeA[Arr[_[_, _], _, _], E, A, B] = Fix[AEnvT[E, Arr, ?[_, _], ?, ?], A, B]

  def cataNT[S[_[_, _], _, _], F[_, _]](
    alg: Alg[S, F]
  )(implicit S: ArFunctor[S]): Fix[S, ?, ?] ~~> F =
    new (Fix[S, ?, ?] ~~> F) { self =>

      def apply[A, B](f: Fix[S, A, B]): F[A, B] =
        alg(S.armap(self)(f.unFix))
    }

  def hyloNT[S[_[_, _], _, _], F[_, _], G[_, _]](
    coAlg: Coalg[S, F],
    alg: Alg[S, G]
  )(
    implicit S: ArFunctor[S]
  ): F ~~> G = new (F ~~> G) { self =>

    def apply[A, B](fa: F[A, B]): G[A, B] =
      alg(S.armap(self)(coAlg(fa)))
  }
}
