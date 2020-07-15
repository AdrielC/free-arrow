package com.adrielc.arrow

import cats.arrow.{Arrow, ArrowChoice}

package object free {

  type FA[F[_, _], A, B]  = FreeA[Arrow, F, A, B]

  type FAC[F[_, _], A, B] = FreeA[ArrowChoice, F, A, B]

  type FAZ[F[_, _], A, B] = FreeA[ArrowZero, F, A, B]

  type FAP[F[_, _], A, B] = FreeA[ArrowPlus, F, A, B]

  type FACZ[F[_, _], A, B] = FreeA[ArrowChoiceZero, F, A, B]

  type FACP[F[_, _], A, B] = FreeA[ArrowChoicePlus, F, A, B]

  val goL: Either[Unit, Unit] = Left(())

  val goR: Either[Unit, Unit] = Right(())

  object implicits {

    implicit class ArrOps[F[_, _], A, B](private val f: F[A, B]) extends AnyVal {

      @inline def `unary_~`: FA[F, A, B] = FreeA.lift(f)
    }

    implicit class FunctionArrOps[A, B](private val f: A => B) extends AnyVal {

      @inline def `unary_~`: FreeA[Arrow, Nothing, A, B] = FreeA.fn(f)
    }
  }

  type LubC[+F[_[_, _]]] = Lub[F, ArrowChoice]
  type LubP[+F[_[_, _]]] = Lub[F, ArrowPlus]
}

package free {

  /** For unifying types between Arrows when mixing capabilities */
  trait Lub[+F[_[_, _]], +G[_[_, _]]] {
    type Lub[f[_, _]] <: F[f] with G[f]
  }
  object Lub extends LubArrow0 {
    type Aux[F[f[_, _]], G[f[_, _]], Ar[f[_, _]]] = Lub[F, G] { type Lub[f[_, _]] = Ar[f] }
    implicit val arr: Lub.Aux[Arrow, Arrow, Arrow] = new Lub[Arrow, Arrow] { type Lub[f[_, _]] = Arrow[f] }
  }
  trait LubArrow0 extends LubArrow1 {
    implicit def fac[A[f[_, _]] >: ArrowChoice[f] <: Arrow[f]]: Lub.Aux[A, A, A] = new Lub[A, A] { type Lub[f[_, _]] = A[f] }
    implicit def faz[A[f[_, _]] >: ArrowZero[f] <: Arrow[f]]: Lub.Aux[A, A, A] = new Lub[A, A] { type Lub[f[_, _]] = A[f] }
  }
  trait LubArrow1 extends LubArrow2 {
    implicit def fap[A[f[_, _]] >: ArrowPlus[f] <: Arrow[f]]: Lub.Aux[A, A, A] = new Lub[A, A] { type Lub[f[_, _]] = A[f] }
  }
  trait LubArrow2 extends LubArrow3 {
    implicit def facz[A[f[_, _]] >: ArrowChoiceZero[f] <: Arrow[f]]: Lub.Aux[A, A, A] = new Lub[A, A] { type Lub[f[_, _]] = A[f] }
  }
  trait LubArrow3 {
    implicit def facp[A[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f]]: Lub.Aux[A, A, A] = new Lub[A, A] { type Lub[f[_, _]] = A[f] }
  }
}
