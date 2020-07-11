package com.adrielc.arrow.free

import cats.Monoid
import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.data.ConstA

/** Universally quantified arrow for all Free Arrow like structures of shape [[Free]] */
trait ArrowF[Free[_[_, _], _, _]] {

  type Arr[f[_, _]]

  implicit def constArr[M: Monoid]: Arr[ConstA[M, ?, ?]]

  implicit def arrow[F[_, _]]: Arr[Free[F, ?, ?]]

  @inline def lift[F[_, _], A, B](fab: F[A, B]): Free[F, A, B]
}

object ArrowF {

  type Aux[Free[_[_, _], _, _], A[_[_, _]]] = ArrowF[Free] { type Arr[f[_, _]] = A[f] }

  def apply[Free[_[_, _], _, _]](implicit AK: ArrowF[Free]): ArrowF[Free] = AK

  implicit val freeArrowChoiceArrowChoiceK: ArrowF.Aux[FreeArrowChoice, ArrowChoice] = new ArrowF[FreeArrowChoice] {
    override type Arr[f[_, _]] = ArrowChoice[f]
    def constArr[M: Monoid]: Arr[ConstA[M, ?, ?]] = ConstA.arrowChoiceConst
    def arrow[F[_, _]]: Arr[FreeArrowChoice[F, ?, ?]] = FAC.freeArrArrowChoice
    def lift[F[_, _], A, B](fab: F[A, B]): FreeArrowChoice[F, A, B] = FAC.lift(fab)
  }

  implicit val freeArrowArrowK: ArrowF.Aux[FreeArrow, Arrow] = new ArrowF[FreeArrow] {
    override type Arr[f[_, _]] = Arrow[f]
    def constArr[M: Monoid]: Arr[ConstA[M, ?, ?]] = ConstA.arrowChoiceConst
    def arrow[F[_, _]]: Arr[FreeArrow[F, ?, ?]] = FA.freeArrArrow
    def lift[F[_, _], A, B](fab: F[A, B]): FreeArrow[F, A, B] = FA.lift(fab)
  }
}
