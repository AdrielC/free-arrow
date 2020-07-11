package com.adrielc.arrow.data

import cats.Monoid
import cats.syntax.monoid._
import com.adrielc.arrow.ArrowChoicePlus

/** Like [[cats.data.Const]] */
case class ConstP[M, -A, +B](getConst: M) {

  def tag[C, D]: ConstP[M, C, D] = this.asInstanceOf[ConstP[M, C, D]]
}

object ConstP {

  implicit def constArrMonoid[V: Monoid, A, B]: Monoid[ConstP[V, A, B]] =
    Monoid.instance(ConstP(Monoid.empty[V]), (a, b) => ConstP(a.getConst |+| b.getConst))

  implicit def constArrow[V: Monoid]: ArrowChoicePlus[ConstP[V, ?, ?]] = new ArrowChoicePlus[ConstP[V, ?, ?]] {

    def zeroArrow[B, C]: ConstP[V, B, C] = constArrMonoid[V, B, C].empty

    def lift[A, B](f: A => B): ConstP[V, A, B] = constArrMonoid[V, A, B].empty

    def choose[A, B, C, D](f: ConstP[V, A, C])(g: ConstP[V, B, D]): ConstP[V, Either[A, B], Either[C, D]] = (f |+| g.tag).tag

    def compose[A, B, C](f: ConstP[V, B, C], g: ConstP[V, A, B]): ConstP[V, A, C] = (f |+| g.tag).tag

    def plus[A, B](f: ConstP[V, A, B], g: ConstP[V, A, B]): ConstP[V, A, B] = (f |+| g.tag).tag

    def first[A, B, C](fa: ConstP[V, A, B]): ConstP[V, (A, C), (B, C)] = fa.tag
  }
}