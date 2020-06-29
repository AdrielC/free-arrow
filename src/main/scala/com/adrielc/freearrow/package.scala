package com.adrielc


package object freearrow {

  type ~~>[-F[_, _], +G[_, _]] = FunctionBiK[F, G]

  implicit class ArrOps[F[_, _], A, B](private val f: F[A, B]) extends AnyVal {

    def ar(): FreeArrow[F, A, B] = FreeArrow.lift(f)

    def ch(): FreeArrowChoice[F, A, B] = FreeArrowChoice.lift(f)
  }
}
