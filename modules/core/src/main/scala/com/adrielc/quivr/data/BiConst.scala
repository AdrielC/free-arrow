package com.adrielc.quivr.data

import cats.Monoid
import cats.kernel.Semigroup
import cats.syntax.monoid._
import com.adrielc.quivr.ArrowChoicePlus
import com.adrielc.quivr.util.BiDistributes

/** Like [[cats.data.Const]] */
case class BiConst[M, -A, +B](getConst: M) {

  def tag[C, D]: BiConst[M, C, D] = this.asInstanceOf[BiConst[M, C, D]]
}

object BiConst {

  implicit def constArrMonoid[V: Monoid, A, B]: Monoid[BiConst[V, A, B]] =
    Monoid.instance(BiConst(Monoid.empty[V]), (a, b) => BiConst(a.getConst |+| b.getConst))

  implicit def constArrow[V: Monoid]: ArrowChoicePlus[BiConst[V, *, *]] = new ArrowChoicePlus[BiConst[V, *, *]] {

    def zeroArrow[B, C]: BiConst[V, B, C] = constArrMonoid[V, B, C].empty

    def lift[A, B](f: A => B): BiConst[V, A, B] = constArrMonoid[V, A, B].empty

    def choose[A, B, C, D](f: BiConst[V, A, C])(g: BiConst[V, B, D]): BiConst[V, Either[A, B], Either[C, D]] = (f |+| g.tag).tag

    def compose[A, B, C](f: BiConst[V, B, C], g: BiConst[V, A, B]): BiConst[V, A, C] = (g |+| f.tag).tag

    def plus[A, B](f: BiConst[V, A, B], g: BiConst[V, A, B]): BiConst[V, A, B] = (f |+| g.tag).tag

    def first[A, B, C](fa: BiConst[V, A, B]): BiConst[V, (A, C), (B, C)] = fa.tag
  }

  implicit def constDistributes[S: Semigroup]: BiDistributes[BiConst[S, *, *], BiConst[S, *, *]] =
    new BiDistributes[BiConst[S, *, *], BiConst[S, *, *]] {
      def dist[A0, A1, B0, B1](pa: BiConst[S, A0, A1], pb: BiConst[S, B0, B1]): BiConst[S, BiConst[S, A0, B0], BiConst[S, A1, B1]] =
        BiConst(pa.getConst |+| pb.getConst)
    }
}