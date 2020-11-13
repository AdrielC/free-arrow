package com.adrielc.quivr

import cats.Eval

package object data {

  type EnvA[E, +F[_, _], A, B] = BiTuple2K[BiConst[Eval[E], *, *], F, A, B]
  object EnvA {

    def apply[E, F[_, _], A, B](env: => E, fab: F[A, B]): EnvA[E, F, A, B] = EnvA[F](env)(fab)

    def apply[F[_, _]]: MkEnvA[F] = new MkEnvA[F]

    final class MkEnvA[F[_, _]] private[EnvA] {
      def apply[E](e: => E): F ~~> EnvA[E, F, *, *] = new (F ~~> EnvA[E, F, *, *]) {
        val env = Eval.later(e)
        def apply[A, B](f: F[A, B]): EnvA[E, F, A, B] = BiTuple2K(BiConst(env), f)
      }
    }
  }

  type :&:[A,B] = (Eval[A], Eval[B])

  def lazyTuple[A, B](a: => A, b: => Eval[B]): A :&: B = (Eval.later(a), Eval.defer(b))
}

package data {
  
  class or[F[_, _], G[_, _]] {
    type T[A, B] = BiEitherK[F, G, A, B]
  }
}
