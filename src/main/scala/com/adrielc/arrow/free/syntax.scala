package com.adrielc.arrow.free

import com.adrielc.arrow.free.methods.{ArrowF, FreeArrowLike}

object syntax {

  implicit class ArrOps[F[_, _], A, B](private val f: F[A, B]) extends AnyVal {

    @inline def lift[Free[_[_, _], _, _]: ArrowF]: Free[F, A, B] = ArrowF[Free].lift(f)

    @inline def A: FA[F, A, B] = lift[FreeArrow]

    @inline def C: FreeArrowChoice[F, A, B] = lift[FreeArrowChoice]

    @inline def P: FreeArrowPlus[F, A, B] = lift[FreeArrowPlus]
  }

  implicit class FunctionArrOps[A, B](private val f: A => B) extends AnyVal {

    @inline def arr[Free[_[_, _], _, _]: ArrowF]: Free[Nothing, A, B] = ArrowF[Free].arr(f)

    @inline def >>>[Free[f[_, _], a, b] <: FreeArrowLike[Free, Nothing, f, a, b], F[_, _], C](fbc: FreeArrowLike[Free, Nothing, F, B, C]): Free[F, A, C] = fbc <<< f

    @inline def <<<[Free[f[_, _], a, b] <: FreeArrowLike[Free, Nothing, f, a, b], F[_, _], C](fbc: FreeArrowLike[Free, Nothing, F, C, A]): Free[F, C, B] = fbc >>> f
  }
}