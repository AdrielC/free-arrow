package com.adrielc.quivr
package recursion

import com.adrielc.quivr.free.FreeArrow

trait ArrFunctor[Ar[_[_, _], _, _]] {

  def arrmap[F[_, _], G[_, _]](nt: F ~~> G): Ar[F, *, *] ~~> Ar[G, *, *]
}

object ArrFunctor {

  final implicit class ArFunctorOps[Ar[_[_, _], _, _], F[_, _], A, B](private val ar: Ar[F, A, B]) extends AnyVal {
    def armap[G[_, _]](nt: F ~~> G)(implicit F: ArrFunctor[Ar]): Ar[G, A, B] = F.arrmap(nt)(ar)
  }

  implicit def freeAArFunctor[Ar[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f]: FreeArrow.->>]: ArrFunctor[FreeArrow[Ar, *[_, _], *, *]] =
    new ArrFunctor[FreeArrow[Ar, *[_, _], *, *]] {

      def arrmap[F[_, _], G[_, _]](nt: F ~~> G): FreeArrow[Ar, F, *, *] ~~> FreeArrow[Ar, G, *, *] =
        new (FreeArrow[Ar, F, *, *] ~~> FreeArrow[Ar, G, *, *]) {

          def apply[A, B](f: FreeArrow[Ar, F, A, B]): FreeArrow[Ar, G, A, B] = f.compile(nt)
        }
    }
}