package com.adrielc.quivr.recursion

import com.adrielc.quivr.~~>

final case class ArrEnvT[E, Arr[_[_, _], _, _], F[_, _], A, B](ask: E, fa: Arr[F, A, B])

object ArrEnvT {

  implicit def envtTArFunctor[E, Ar[_[_, _], _, _]](
    implicit F: ArrFunctor[Ar]
  ): ArrFunctor[ArrEnvT[E, Ar, *[_, _], *, *]] =
    new ArrFunctor[ArrEnvT[E, Ar, *[_, _], *, *]] {

      def arrmap[M[_, _], N[_, _]](nt: M ~~> N): ArrEnvT[E, Ar, M, *, *] ~~> ArrEnvT[E, Ar, N, *, *] =
        new (ArrEnvT[E, Ar, M, *, *] ~~> ArrEnvT[E, Ar, N, *, *]) {
          def apply[I, J](fm: ArrEnvT[E, Ar, M, I, J]) = ArrEnvT(fm.ask, F.arrmap(nt)(fm.fa))
        }
    }
}