package com.adrielc.arrow.recursion

import cats.arrow.Arrow
import com.adrielc.arrow.free.FreeA
import com.adrielc.arrow.ArrowChoicePlus
import com.adrielc.arrow.data.~~>

trait ArFunctor[Ar[_[_, _], _, _]] {

  def armap[F[_, _], G[_, _]](nt: F ~~> G): Ar[F, ?, ?] ~~> Ar[G, ?, ?]
}

object ArFunctor {

  final implicit class ArFunctorOps[Ar[_[_, _], _, _], F[_, _], A, B](private val ar: Ar[F, A, B]) extends AnyVal {
    def armap[G[_, _]](nt: F ~~> G)(implicit F: ArFunctor[Ar]): Ar[G, A, B] = F.armap(nt)(ar)
  }

  implicit def freeAArFunctor[Ar[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f]]: ArFunctor[FreeA[Ar, ?[_, _], ?, ?]] =
    new ArFunctor[FreeA[Ar, ?[_, _], ?, ?]] {

      def armap[F[_, _], G[_, _]](nt: F ~~> G): FreeA[Ar, F, ?, ?] ~~> FreeA[Ar, G, ?, ?] =
        new (FreeA[Ar, F, ?, ?] ~~> FreeA[Ar, G, ?, ?]) {

          def apply[A, B](f: FreeA[Ar, F, A, B]): FreeA[Ar, G, A, B] = f.compile(nt)
        }
    }
}

final case class Fix[Ar[_[_, _], _, _], A, B](unFix: Ar[Fix[Ar, ?, ?], A, B])

final case class AEnvT[E, Ar[_[_, _], _, _], G[_, _], A, B](ask: E, fa: Ar[G, A, B])

object AEnvT {

  implicit def aEnvtTArFunctor[E, Ar[_[_, _], _, _]](
    implicit F: ArFunctor[Ar]
  ): ArFunctor[AEnvT[E, Ar, ?[_, _], ?, ?]] =
    new ArFunctor[AEnvT[E, Ar, ?[_, _], ?, ?]] {

      def armap[M[_, _], N[_, _]](nt: M ~~> N): AEnvT[E, Ar, M, ?, ?] ~~> AEnvT[E, Ar, N, ?, ?] =
        new (AEnvT[E, Ar, M, ?, ?] ~~> AEnvT[E, Ar, N, ?, ?]) {
          def apply[I, J](fm: AEnvT[E, Ar, M, I, J]) = AEnvT(fm.ask, F.armap(nt)(fm.fa))
        }
    }
}