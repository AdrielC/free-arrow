package com.adrielc.quivr

import com.adrielc.quivr.data.{BiEitherK, EnvA}

package object free {

  val FA = FreeArrow
  val FC = FreeCompose

  /** All levels of FreeArrow capability */
  type FA[+F[_, _], A, B] = FreeArrow[AR, F, A, B]
  type FAC[+F[_, _], A, B] = FreeArrow[AC, F, A, B]
  type FAP[+F[_, _], A, B] = FreeArrow[AP, F, A, B]
  type FAZ[+F[_, _], A, B] = FreeArrow[AZ, F, A, B]
  type FACZ[+F[_, _], A, B] = FreeArrow[ACZ, F, A, B]
  type FACP[+F[_, _], A, B] = FreeArrow[ACP, F, A, B]


  /** Specialized types of [[FreeArrow]] that correspond to method symbols */
  type >>>[A, B] = FA[Nothing, A, B]

  /** [[FreeArrow.justLeft]] */
  type ^|-[L, R] = FACZ[Nothing, Either[L, R], L]

  /** [[FreeArrow.justRight]] */
  type -|^[L, R] = FACZ[Nothing, Either[L, R], R]

  /** [[FreeArrow.zeroArrow]] */
  type ~@~[A, B] = FAZ[Nothing, A, B]


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
