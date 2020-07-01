package com.adrielc.arrow.data

import cats.Monoid
import cats.arrow.ArrowChoice
import cats.syntax.monoid._

/** Like [[cats.data.Const]] */
case class ConstArr[V, -A, +B](getConst: V) {

  def tag[C, D]: ConstArr[V, C, D] = this.asInstanceOf[ConstArr[V, C, D]]
}

object ConstArr {

  implicit def constArrMonoid[V: Monoid, A, B]: Monoid[ConstArr[V, A, B]] =
    Monoid.instance(ConstArr(Monoid.empty[V]), (a, b) => ConstArr(a.getConst |+| b.getConst))

  implicit def arrowChoiceConst[V: Monoid]: ArrowChoice[ConstArr[V, ?, ?]] = new ArrowChoice[ConstArr[V, ?, ?]] {

    def choose[A, B, C, D](f: ConstArr[V, A, C])(g: ConstArr[V, B, D]): ConstArr[V, Either[A, B], Either[C, D]] = (f |+| g.tag).tag

    def lift[A, B](f: A => B): ConstArr[V, A, B] = constArrMonoid[V, A, B].empty

    def compose[A, B, C](f: ConstArr[V, B, C], g: ConstArr[V, A, B]): ConstArr[V, A, C] = (f |+| g.tag).tag

    def first[A, B, C](fa: ConstArr[V, A, B]): ConstArr[V, (A, C), (B, C)] = fa.tag
  }
}