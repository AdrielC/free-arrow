package com.adrielc.arrow

package object free {

  type FA[F[_, _], A, B] = FreeArrow[F, A, B]
  val FA = FreeArrow

  type FAC[F[_, _], A, B] = FreeArrowChoice[F, A, B]
  val FAC = FreeArrowChoice

  type FAP[F[_, _], A, B] = FreeArrowPlus[F, A, B]
  val FAP = FreeArrowPlus
}
