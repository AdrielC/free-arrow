package com.adrielc.arrows

import cats.Monoid
import cats.arrow.ArrowChoice
import cats.implicits._

case class ConstArr[V, -A, +B](getConst: V) {

  def tag[C, D]: ConstArr[V, C, D] = this.asInstanceOf[ConstArr[V, C, D]]
}

object ConstArr {

  implicit def arrowChoiceConst[V : Monoid]: ArrowChoice[ConstArr[V, ?, ?]] = new ArrowChoice[ConstArr[V, ?, ?]] {

    def choose[A, B, C, D](f: ConstArr[V, A, C])(g: ConstArr[V, B, D]): ConstArr[V, Either[A, B], Either[C, D]] = ConstArr(g.getConst |+| f.getConst)

    def lift[A, B](f: A => B): ConstArr[V, A, B] = ConstArr(Monoid.empty)

    def compose[A, B, C](f: ConstArr[V, B, C], g: ConstArr[V, A, B]): ConstArr[V, A, C] = ConstArr(g.getConst |+| f.getConst)

    def first[A, B, C](fa: ConstArr[V, A, B]): ConstArr[V, (A, C), (B, C)] = fa.tag
  }
}