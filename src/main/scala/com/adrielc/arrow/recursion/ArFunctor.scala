package com.adrielc.arrow.recursion

import cats.arrow.Arrow
import com.adrielc.arrow.free.FreeA
import com.adrielc.arrow.~~>

trait ArFunctor[Ar[_[_, _], _, _]] {

  def armap[F[_, _], G[_, _]](nt: F ~~> G): Ar[F, ?, ?] ~~> Ar[G, ?, ?]
}

object ArFunctor {

  def freeAArFunctor[Ar[f[_, _]] <: Arrow[f]]: ArFunctor[FreeA[Ar, ?[_, _], ?, ?]] =
    new ArFunctor[FreeA[Ar, ?[_, _], ?, ?]] {

      override def armap[F[_, _], G[_, _]](nt: F ~~> G): FreeA[Ar, F, ?, ?] ~~> FreeA[Ar, G, ?, ?] = ???
    }
}

final case class Fix[Ar[_[_, _], _, _], A, B](unFix: Ar[Fix[Ar, ?, ?], A, B])

final case class ArEnvT[E, Ar[_[_, _], _, _], G[_, _], I, J](ask: E, fa: Ar[G, I, J])

object ArEnvT {

  implicit def arFunctor[E, Ar[_[_, _], _, _]](
    implicit F: ArFunctor[Ar]
  ): ArFunctor[ArEnvT[E, Ar, ?[_, _], ?, ?]] =
    new ArFunctor[ArEnvT[E, Ar, ?[_, _], ?, ?]] {

      def armap[M[_, _], N[_, _]](nt: M ~~> N): ArEnvT[E, Ar, M, ?, ?] ~~> ArEnvT[E, Ar, N, ?, ?] =
        new (ArEnvT[E, Ar, M, ?, ?] ~~> ArEnvT[E, Ar, N, ?, ?]) {
          def apply[I, J](fm: ArEnvT[E, Ar, M, I, J]) = ArEnvT(fm.ask, F.armap(nt)(fm.fa))
        }
    }
}