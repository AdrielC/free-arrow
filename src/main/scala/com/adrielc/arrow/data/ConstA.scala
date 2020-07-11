package com.adrielc.arrow.data

import cats.Monoid
import cats.arrow.ArrowChoice
import cats.syntax.monoid._

/** Like [[cats.data.Const]] */
case class ConstA[M, -A, +B](getConst: M) {

  def tag[C, D]: ConstA[M, C, D] = this.asInstanceOf[ConstA[M, C, D]]
}

object ConstA {

  implicit def constArrMonoid[V: Monoid, A, B]: Monoid[ConstA[V, A, B]] =
    Monoid.instance(ConstA(Monoid.empty[V]), (a, b) => ConstA(a.getConst |+| b.getConst))

  implicit def arrowChoiceConst[V: Monoid]: ArrowChoice[ConstA[V, ?, ?]] = new ArrowChoice[ConstA[V, ?, ?]] {

    def choose[A, B, C, D](f: ConstA[V, A, C])(g: ConstA[V, B, D]): ConstA[V, Either[A, B], Either[C, D]] = (f |+| g.tag).tag

    def lift[A, B](f: A => B): ConstA[V, A, B] = constArrMonoid[V, A, B].empty

    def compose[A, B, C](f: ConstA[V, B, C], g: ConstA[V, A, B]): ConstA[V, A, C] = (f |+| g.tag).tag

    def first[A, B, C](fa: ConstA[V, A, B]): ConstA[V, (A, C), (B, C)] = fa.tag
  }
}