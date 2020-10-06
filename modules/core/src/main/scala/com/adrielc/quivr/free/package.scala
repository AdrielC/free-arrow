package com.adrielc.quivr

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.quivr.data.{BiEitherK, EnvA}

package object free {

  val arrow = FreeArrow
  val compose = FreeCompose

  /** Arrow hierarchy supported by FreeArrow */
  type AR[f[_, _]] = Arrow[f]
  type AC[f[_, _]] = ArrowChoice[f]
  type AP[f[_, _]] = ArrowPlus[f]
  type AZ[f[_, _]] = ArrowZero[f]
  type ACZ[f[_, _]] = ArrowChoiceZero[f]
  type ACP[f[_, _]] = ArrowChoicePlus[f]


  /** All levels of FreeArrow capability */
  type FA[+F[_, _], A, B] = FreeArrow[AR, F, A, B]
  type FC[+F[_, _], A, B] = FreeArrow[AC, F, A, B]
  type FP[+F[_, _], A, B] = FreeArrow[AP, F, A, B]
  type FZ[+F[_, _], A, B] = FreeArrow[AZ, F, A, B]
  type FCZ[+F[_, _], A, B] = FreeArrow[ACZ, F, A, B]
  type FCP[+F[_, _], A, B] = FreeArrow[ACP, F, A, B]


  /** Specialized types of [[FreeArrow]] that correspond to method symbols */
  type >>>[A, B] = FA[Nothing, A, B]

  /** [[FreeArrow.justLeft]] */
  type ^|-[L, R] = FCZ[Nothing, Either[L, R], L]

  /** [[FreeArrow.justRight]] */
  type -|^[L, R] = FCZ[Nothing, Either[L, R], R]

  /** [[FreeArrow.zeroArrow]] */
  type ~@~[A, B] = FZ[Nothing, A, B]


  /** Rebuild/Optimize FreeA
   *
   * Given some summary [[M]] and an [[F]] for any `A` and `B`, create a `FreeA[Arrow, F, A, B]`
   *
   * Isomorphic to `(M, F[A, B]) => FreeA[Arrow, F, A, B]`
   * */
  type |~>[M, -R[f[_, _]] >: ACP[f] <: AR[f], F[_, _]] = EnvA[M, F, *, *] ~~> FreeArrow[R, F, *, *]

  type EitherFreeA[-R[f[_, _]] >: ACP[f] <: AR[f], +F[_, _], +G[_, _], A, B] = FreeArrow[R, BiEitherK[F, G, *, *], A, B]

  type || = Either[Unit, Unit]
  type ||| = Either[Either[Unit, Unit], Unit]

  val `^|`  : ||  = Left(())
  val `|^`  : ||  = Right(())
  val `||^` : ||| = Right(())
  val `|^|` : ||| = Left(Right(()))
  val `^||` : ||| = Left(Left(()))
}
