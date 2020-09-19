package com.adrielc.quivr

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.quivr.data.{BiEitherK, EnvA, ~~>}

package object free {

  /** Arrow hierarchy supported by FreeArrow */
  type AR[f[_, _]] = Arrow[f]
  type AC[f[_, _]] = ArrowChoice[f]
  type AP[f[_, _]] = ArrowPlus[f]
  type AZ[f[_, _]] = ArrowZero[f]
  type ACZ[f[_, _]] = ArrowChoiceZero[f]
  type ACP[f[_, _]] = ArrowChoicePlus[f]


  /** All levels of FreeArrow capability */
  type FA[+F[_, _], A, B]   = FreeA[Arrow,           F, A, B]
  type FC[+F[_, _], A, B]   = FreeA[ArrowChoice,     F, A, B]
  type FP[+F[_, _], A, B]   = FreeA[ArrowPlus,       F, A, B]
  type FZ[+F[_, _], A, B]   = FreeA[ArrowZero,       F, A, B]
  type FCZ[+F[_, _], A, B]  = FreeA[ArrowChoiceZero, F, A, B]
  type FCP[+F[_, _], A, B]  = FreeA[ArrowChoicePlus, F, A, B]


  /** Specialized types of [[FreeA]] that correspond to method symbols */
  type >>>[A, B] = FA[Nothing, A, B]

  /** [[FreeA.justLeft]] */
  type ^|-[L, R] = FCZ[Nothing, Either[L, R], L]

  /** [[FreeA.justRight]] */
  type -|^[L, R] = FCZ[Nothing, Either[L, R], R]

  /** [[FreeA.zeroArrow]] */
  type ~@~[A, B] = FZ[Nothing, A, B]


  /** Rebuild/Optimize FreeA
   *
   * Given some summary [[M]] and an [[F]] for any `A` and `B`, create a `FreeA[Arrow, F, A, B]`
   *
   * Isomorphic to `(M, F[A, B]) => FreeA[Arrow, F, A, B]`
   * */
  type |~>[M, -R[f[_, _]] >: ACP[f] <: AR[f], F[_, _]] = EnvA[M, F, *, *] ~~> FreeA[R, F, *, *]

  type EitherFreeA[-R[f[_, _]] >: ACP[f] <: AR[f], +F[_, _], +G[_, _], A, B] = FreeA[R, BiEitherK[F, G, *, *], A, B]

  type || = Either[Unit, Unit]
  type ||| = Either[Either[Unit, Unit], Unit]

  val `^|`  : ||  = Left(())
  val `|^`  : ||  = Right(())
  val `||^` : ||| = Right(())
  val `|^|` : ||| = Left(Right(()))
  val `^||` : ||| = Left(Left(()))

  type |||@[+F[f[_, _]] >: ACP[f]] = Lub[F, AC, ACP]
  type <+>@[+F[f[_, _]] >: ACP[f]] = Lub[F, AP, ACP]
}

package free {

  /** For unifying types between Arrows when mixing FreeA capabilities */
  trait Lub[+F[f[_, _]] >: B[f], +G[f[_, _]] >: B[f], -B[_[_, _]]] {
    type Lub[f[_, _]] >: B[f] <: G[f] with F[f]
  }
  object Lub extends LubArrow0 {
    type Aux[+F[f[_, _]] >: B[f], +G[f[_, _]] >: B[f], -B[_[_, _]], O[_[_, _]]] = Lub[F, G, B] { type Lub[f[_, _]] = O[f] }
    implicit val ar: Lub.Aux[AR, AR, ACP, AR] = new Lub[AR, AR, ACP] { type Lub[f[_, _]] = AR[f] }
  }
  trait LubArrow0 extends LubArrow1 {
    implicit val az: Lub.Aux[AZ, AZ, ACP, AZ] = new Lub[AZ, AZ, ACP] { type Lub[f[_, _]] = AZ[f] }
    implicit val ac: Lub.Aux[AC, AC, ACP, AC] = new Lub[AC, AC, ACP] { type Lub[f[_, _]] = AC[f] }
  }
  trait LubArrow1 extends LubArrow2 {
    implicit val ap: Lub.Aux[AP, AP, ACP, AP] = new Lub[AP, AP, ACP] { type Lub[f[_, _]] = AP[f] }
  }
  trait LubArrow2 extends LubArrow3 {
    implicit val acz: Lub.Aux[ACZ, ACZ, ACP, ACZ] = new Lub[ACZ, ACZ, ACP] { type Lub[f[_, _]] = ACZ[f] }
  }
  trait LubArrow3 {
    implicit val acp: Lub.Aux[ACP, ACP, ACP, ACP] = new Lub[ACP, ACP, ACP] { type Lub[f[_, _]] = ACP[f] }
  }
}
