package com.adrielc.arrow

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.free.FreeA.{Fn, Lift}
import com.adrielc.arrow.recursion.CoalgEnv

package object free {
  import FreeA.{fn, lift, Id, Zero, arrowInstance => ~>}

  /**
   * A Free arrow with context [[F]] and capability of [[Arrow]]
   *
   * The following operations are supported by [[FA]]:
   *
   * [[FreeA.andThen]]  alias   [[FreeA.>>>]]
   * [[FreeA.compose]]  alias   [[FreeA.<<<]]
   * [[FreeA.merge]]    alias   [[FreeA.|||]]
   * [[FreeA.split]]    alias   [[FreeA.***]]
   * [[FreeA.first]]
   * [[FreeA.second]]
   * [[FreeA.second]]
   *
   * Any other composition methods will shift the context
   * to a more powerful subtype of [[Arrow]]
   *
   *
   */
  type FA[F[_, _], A, B]    = FreeA[Arrow, F, A, B]
  type FAC[F[_, _], A, B]   = FreeA[ArrowChoice, F, A, B]
  type FAP[F[_, _], A, B]   = FreeA[ArrowPlus, F, A, B]
  type FAZ[F[_, _], A, B]   = FreeA[ArrowZero, F, A, B]
  type FACZ[F[_, _], A, B]  = FreeA[ArrowChoiceZero, F, A, B]
  type FACP[F[_, _], A, B]  = FreeA[ArrowChoicePlus, F, A, B]



  // Free Constructors

  /**
   * [[Arrow.id]]
   * */
  type >>>[A, B] = FA[Nothing, A, B]
  object >>> {
    def apply[A]: A >>> A = Id()
  }

  type >>^[F[_, _], A, B]   = FA[F, A, B]
  object >>^ {
    def apply[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = Lift(fab)
  }

  object >^ {
    def apply[A, B](f: A => B): A >>> B = Fn(cats.data.AndThen(f))
  }

  /**
   * [[ArrowZero.zeroArrow]]
   * */
  type ~@[A, B] = FAZ[Nothing, A, B]
  object ~@ {
    def apply[A, B]: A ~@ B = Zero()
  }

  /**
   * [[ArrowChoiceZero.justLeft]]
   * */
  type ^|-[L, R] = FACZ[Nothing, Either[L, R], L]
  object ^|- {
    def apply[L, R]: L ^|- R = ~>[ArrowChoiceZero, Nothing].justLeft
  }

  /**
   * [[ArrowChoiceZero.right]]
   * */
  type -|^[L, R] = FACZ[Nothing, Either[L, R], R]
  object -|^ {
    def apply[L, R]: L -|^ R = ~>[ArrowChoiceZero, Nothing].justRight
  }





  /** Rebuild/Optimize FreeA
   *
   * Given some environment [[E]] and an [[F]] for any `A` and `B`, create a `FreeA[Arrow, F, A, B]`
   *
   * Isomorphic to `(E, F[A, B]) => FreeA[Arrow, F, A, B]`
   * */
  type |~>[M, Arr[f[_, _]] <: Arrow[f], F[_, _]] = CoalgEnv[λ[(f[_, _], a, b) => FreeA[Arr, f, a, b]], F, M]


  val `<|`: Either[Unit, Unit] = Left(())

  val `|>`: Either[Unit, Unit] = Right(())

  object implicits {

    implicit class ArrOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {

      @inline def `unary_~`: FA[F, A, B] = lift(fab)
    }

    implicit class FunctionArrOps[A, B](private val f: A => B) extends AnyVal {

      @inline def `unary_~`: FreeA[Arrow, Nothing, A, B] = fn(f)

      @inline def >>>[Arr[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f], F[_, _], C](fbc: FreeA[Arr, F, B, C]): FreeA[Arr, F, A, C] = fbc <^ f

      @inline def >>^[F[_, _], C](fbc: F[B, C]): FA[F, A, C] = lift(fbc) <^ f

      @inline def <<<[Arr[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f], F[_, _], C](fbc: FreeA[Arr, F, C, A]): FreeA[Arr, F, C, B] = fbc >^ f

      @inline def <<^[F[_, _], C](fca: F[C, A]): FA[F, C, B] = lift(fca) >^ f
    }
  }

  type |||@[+F[f[_, _]] >: ACP[f] <: Arrow[f]] = Lub[F, AC, ACP]
  type <+>@[+F[f[_, _]] >: ACP[f]] = Lub[F, AP, ACP]
  type ^|-@[+F[f[_, _]] >: ACP[f]] = Lub[F, ACZ, ACP]
  type |&|@[+F[f[_, _]] >: ACP[f]] = Lub[F, ACP, ACP]

  type AR[f[_, _]] = Arrow[f]
  type AC[f[_, _]] = ArrowChoice[f]
  type AP[f[_, _]] = ArrowPlus[f]
  type AZ[f[_, _]] = ArrowZero[f]
  type ACZ[f[_, _]] = ArrowChoiceZero[f]
  type ACP[f[_, _]] = ArrowChoicePlus[f]
}

package free {

  /** For unifying types between Arrows when mixing FreeA capabilities */
  trait Lub[+F[f[_, _]] >: B[f], +G[f[_, _]] >: B[f], B[_[_, _]]] {
    type Lub[f[_, _]] >: B[f] <: G[f] with F[f]
  }
  object Lub extends LubArrow0 {
    type Aux[F[f[_, _]] >: ACP[f], G[f[_, _]] >: ACP[f], Ar[f[_, _]]] = Lub[F, G, ACP] { type Lub[f[_, _]] = Ar[f] }
    implicit val ar: Lub.Aux[AR, AR, AR] = new Lub[AR, AR, ACP] { type Lub[f[_, _]] = AR[f] }
  }
  trait LubArrow0 extends LubArrow1 {
    implicit val az: Lub.Aux[AZ, AZ, AZ] = new Lub[AZ, AZ, ACP] { type Lub[f[_, _]] = AZ[f] }
    implicit val ac: Lub.Aux[AC, AC, AC] = new Lub[AC, AC, ACP] { type Lub[f[_, _]] = AC[f] }
  }
  trait LubArrow1 extends LubArrow2 {
    implicit val ap: Lub.Aux[AP, AP, AP] = new Lub[AP, AP, ACP] { type Lub[f[_, _]] = AP[f] }
  }
  trait LubArrow2 extends LubArrow3 {
    implicit val acz: Lub.Aux[ACZ, ACZ, ACZ] = new Lub[ACZ, ACZ, ACP] { type Lub[f[_, _]] = ACZ[f] }
  }
  trait LubArrow3 {
    implicit val acp: Lub.Aux[ACP, ACP, ACP] = new Lub[ACP, ACP, ACP] { type Lub[f[_, _]] = ACP[f] }
  }
}
