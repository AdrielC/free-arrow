package com.adrielc.arrow.data

import cats.Monoid
import cats.syntax.monoid._
import com.adrielc.arrow.ArrowChoicePlus

/** Like [[cats.data.Const]] */
case class ConstA[M, -A, +B](getConst: M) {

  def tag[C, D]: ConstA[M, C, D] = this.asInstanceOf[ConstA[M, C, D]]
}

object ConstA {

  implicit def constArrMonoid[V: Monoid, A, B]: Monoid[ConstA[V, A, B]] =
    Monoid.instance(ConstA(Monoid.empty[V]), (a, b) => ConstA(a.getConst |+| b.getConst))

  implicit def constArrow[V: Monoid]: ArrowChoicePlus[ConstA[V, ?, ?]] = new ArrowChoicePlus[ConstA[V, ?, ?]] {

    def zeroArrow[B, C]: ConstA[V, B, C] = constArrMonoid[V, B, C].empty

    def lift[A, B](f: A => B): ConstA[V, A, B] = constArrMonoid[V, A, B].empty

    def choose[A, B, C, D](f: ConstA[V, A, C])(g: ConstA[V, B, D]): ConstA[V, Either[A, B], Either[C, D]] = (f |+| g.tag).tag

    def compose[A, B, C](f: ConstA[V, B, C], g: ConstA[V, A, B]): ConstA[V, A, C] = (f |+| g.tag).tag

    def plus[A, B](f: ConstA[V, A, B], g: ConstA[V, A, B]): ConstA[V, A, B] = (f |+| g.tag).tag

    def first[A, B, C](fa: ConstA[V, A, B]): ConstA[V, (A, C), (B, C)] = fa.tag
  }
}