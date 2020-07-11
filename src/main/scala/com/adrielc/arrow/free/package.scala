package com.adrielc.arrow

import com.adrielc.arrow.free.methods.FreeArrowLike

package object free {

  type FA[F[_, _], A, B] = FreeArrow[F, A, B]
  val FA = FreeArrow

  type FAC[F[_, _], A, B] = FreeArrowChoice[F, A, B]
  val FAC = FreeArrowChoice

  object syntax {

    implicit class ArrOps[F[_, _], A, B](private val f: F[A, B]) extends AnyVal {

      @inline def lift[Free[_[_, _], _, _]: ArrowF]: Free[F, A, B] = ArrowF[Free].lift(f)

      @inline def ar(): FreeArrow[F, A, B] = lift[FreeArrow]

      @inline def ch(): FreeArrowChoice[F, A, B] = lift[FreeArrowChoice]
    }

    implicit class FunctionArrOps[A, B](private val f: A => B) extends AnyVal {

      @inline def ^>>[Free[f[_, _], a, b] <: FreeArrowLike[Free, Nothing, f, a, b], F[_, _], C](fbc: FreeArrowLike[Free, Nothing, F, B, C]): Free[F, A, C] = fbc <<^ f

      @inline def ^<<[Free[f[_, _], a, b] <: FreeArrowLike[Free, Nothing, f, a, b], F[_, _], C](fbc: FreeArrowLike[Free, Nothing, F, C, A]): Free[F, C, B] = fbc >>^ f
    }
  }
}
