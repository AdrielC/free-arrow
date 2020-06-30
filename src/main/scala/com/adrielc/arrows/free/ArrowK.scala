package com.adrielc.arrows.free

import cats.arrow.{Arrow, ArrowChoice}


/** Universally quantified arrow for all Free Arrow like structures of shape [[FA]] */
trait ArrowK[FA[_[_, _], _, _]] {

  def arrow[F[_, _]]: Arrow[FA[F, ?, ?]]

  def lift[F[_, _], A, B](fab: F[A, B]): FA[F, A, B]
}

object ArrowK {

  implicit val freeArrowArrowK: ArrowK[FreeArrow] = new ArrowK[FreeArrow] {
    def arrow[F[_, _]]: Arrow[FreeArrow[F, ?, ?]] = FreeArrow.freeArrArrow
    def lift[F[_, _], A, B](fab: F[A, B]): FreeArrow[F, A, B] = FreeArrow.lift(fab)
  }

  /** [[ArrowChoice]] specialization of [[ArrowK]] */
  trait ArrowChoiceK[FAC[_[_, _], _, _]] extends ArrowK[FAC] {

    def arrowChoice[F[_, _]]: ArrowChoice[FAC[F, ?, ?]]

    override def arrow[F[_, _]]: Arrow[FAC[F, ?, ?]] = arrowChoice
  }
  object ArrowChoiceK {

    implicit val freeArrowChoiceArrowK: ArrowChoiceK[FreeArrowChoice] = new ArrowChoiceK[FreeArrowChoice] {
      def arrowChoice[F[_, _]]: ArrowChoice[FreeArrowChoice[F, ?, ?]] = FreeArrowChoice.freeArrArrowChoice
      def lift[F[_, _], A, B](fab: F[A, B]): FreeArrowChoice[F, A, B] = FreeArrowChoice.lift(fab)
    }
  }
}
