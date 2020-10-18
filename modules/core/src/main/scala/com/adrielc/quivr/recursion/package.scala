package com.adrielc.quivr

package object recursion {

  type Alg[A[_[_, _], _, _], F[_, _]]   = A[F, *, *] ~~> F

  type Coalg[A[_[_, _], _, _], F[_, _]] = F ~~> A[F, *, *]

  type CoalgEnv[A[_[_, _], _, _], F[_, _], M] = λ[(α, β) => (M, F[α, β])] ~~> A[F, *, *]

  type Analyze[A[_[_, _], _, _], F[_, _], M] = A[F, *, *] ~>| M

  type CofreeA[Arr[_[_, _], _, _], E, A, B] = ArrFix[ArrEnvT[E, Arr, *[_, _], *, *], A, B]

  def cataNT[S[_[_, _], _, _], F[_, _]](
    alg: Alg[S, F]
  )(implicit S: ArrFunctor[S]): ArrFix[S, *, *] ~~> F =
    new (ArrFix[S, *, *] ~~> F) { self =>

      def apply[A, B](f: ArrFix[S, A, B]): F[A, B] =
        alg(S.arrmap(self)(f.unFix))
    }

  def hyloNT[S[_[_, _], _, _], F[_, _], G[_, _]](
    coAlg: Coalg[S, F],
    alg: Alg[S, G]
  )(implicit S: ArrFunctor[S]): F ~~> G =
    new (F ~~> G) { self =>
      def apply[A, B](fa: F[A, B]): G[A, B] =
        alg(S.arrmap(self)(coAlg(fa)))
    }
}
