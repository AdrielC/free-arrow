package com.adrielc.freearrow

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.freearrow.FreeArrowLike.ArrowK

abstract class FreeArrowLike[FA[+f[_, _], a, b] <: FreeArrowLike[FA, f, a, b], +F[_ ,_], A, B](implicit AK: ArrowK[FA]) extends Product with Serializable {
  self: FA[F, A, B] =>

  final def andThen[C, FF[a, b] >: F[a, b]](fbc: FA[FF, B, C]): FA[FF, A, C] = AK.arrow.andThen(self, fbc)

  final def split[C, D, FF[a, b] >: F[a, b]](fcd: FA[FF, C, D]): FA[FF, (A, C), (B, D)] = AK.arrow.split(self, fcd)

  final def merge[C, FF[a, b] >: F[a, b]](fac: FA[FF, A, C]): FA[FF, A, (B, C)] = AK.arrow.merge(self, fac)

  final def compose[C, FF[a, b] >: F[a, b]](fca: FA[FF, C, A]): FA[FF, C, B] = AK.arrow.compose(self, fca)

  final def id[C]: FA[F, C, C] = AK.arrow[F].id[C]

  final def rmap[C](f: B => C): FA[F, A, C] = AK.arrow.rmap(self)(f)

  final def lmap[C](f: C => A): FA[F, C, B] = AK.arrow.lmap(self)(f)

  final def first[C]: FA[F, (A, C), (B, C)] = AK.arrow.first(self)

  final def second[C]: FA[F, (C, A), (C, B)] = AK.arrow.second(self)

  final def >>>[C, FF[a, b] >: F[a, b]](fbc: FA[FF, B, C]): FA[FF, A, C] = andThen(fbc)

  final def <<<[AA, FF[a, b] >: F[a, b]](fca: FA[FF, AA, A]): FA[FF, AA, B] = compose(fca)

  final def >>^[C, FF[a, b] >: F[a, b]](f: FF[B, C]): FA[FF, A, C] = andThen(AK.liftK(f))

  final def ^<<[AA, FF[a, b] >: F[a, b]](f: FF[AA, A]): FA[FF, AA, B] = compose(AK.liftK(f))

  final def ***[C, D, FF[a, b] >: F[a, b]](fcd: FA[FF, C, D]): FA[FF, (A, C), (B, D)] = split(fcd)

  final def &&&[C, FF[a, b] >: F[a, b]](fac: FA[FF, A, C]): FA[FF, A, (B, C)] = merge(fac)

  final def _1[C](implicit ev: B <:< (C, Any)): FA[F, A, C] = rmap(_._1)

  final def _2[C](implicit ev: B <:< (Any, C)): FA[F, A, C] = rmap(_._2)

  final def in_1: FA[F, A, (A, B)] = id merge self

  final def in_2: FA[F, A, (B, A)] = merge(id)

  final def in: FA[F, A, A] = in_2.rmap(_._2)

  final def -|[FF[a, b] >: F[a, b]](fab: FA[FF, B, Unit]): FA[FF, A, B] = andThen(fab.in)

  final def twice: FA[F, A, (B, B)] = merge(self)

  final def dup: FA[F, A, (B, B)] = rmap(o => (o, o))

  def loopN(n: Int)(implicit ev: B =:= A): FA[F, A, B] = {
    val _ = ev
    val init = self.asInstanceOf[FA[F, B, B]]
    var g = init
    for (_ <- 1 until n) { g = g andThen init }
    g.asInstanceOf[FA[F, A, B]]
  }
}

object FreeArrowLike {

  abstract class FreeArrowChoiceLike[FAC[+f[_, _], a, b] <: FreeArrowChoiceLike[FAC, f, a, b], +F[_ ,_], A, B](implicit AK: ArrowChoiceK[FAC])
    extends FreeArrowLike[FAC, F, A, B] {
    self: FAC[F, A, B] =>

    final def choose[FF[a, b] >: F[a, b], C, D](fcd: FAC[FF, C, D]): FAC[FF, Either[A, C], Either[B, D]] = AK.arrowChoice[FF].choose(self)(fcd)

    final def choice[FF[a, b] >: F[a, b], C](fcb: FAC[FF, C, B]): FAC[FF, Either[A, C], B] = AK.arrowChoice[FF].choice(self, fcb)

    final def left[C]: FAC[F, Either[A, C], Either[B, C]] = AK.arrowChoice.left(self)

    final def right[C]: FAC[F, Either[C, A], Either[C, B]] = AK.arrowChoice.right(self)

    final def |||[FF[a, b] >: F[a, b], C](fcb: FAC[FF, C, B]): FAC[FF, Either[A, C], B] = choice(fcb)

    final def +++[FF[a, b] >: F[a, b], C, D](fcd: FAC[FF, C, D]): FAC[FF, Either[A, C], Either[B, D]] = choose(fcd)
  }


  trait ArrowK[FA[_[_, _], _, _]] {

    def arrow[F[_, _]]: Arrow[FA[F, ?, ?]]

    def liftK[F[_, _], A, B](fab: F[A, B]): FA[F, A, B]
  }
  object ArrowK {

    implicit def freeArrowArrowK: ArrowK[FreeArrow] = new ArrowK[FreeArrow] {
      def arrow[F[_, _]]: Arrow[FreeArrow[F, ?, ?]] = FreeArrow.freeArrArrow[F]
      def liftK[F[_, _], A, B](fab: F[A, B]): FreeArrow[F, A, B] = FreeArrow.lift(fab)
    }
  }

  trait ArrowChoiceK[FAC[_[_, _], _, _]] extends ArrowK[FAC] {

    def arrowChoice[F[_, _]]: ArrowChoice[FAC[F, ?, ?]]

    override def arrow[F[_, _]]: Arrow[FAC[F, ?, ?]] = arrowChoice
  }
  object ArrowChoiceK {

    implicit def freeArrowChoiceArrowK: ArrowChoiceK[FreeArrowChoice] = new ArrowChoiceK[FreeArrowChoice] {
      def arrowChoice[F[_, _]]: ArrowChoice[FreeArrowChoice[F, ?, ?]] = FreeArrowChoice.freeArrArrowChoice[F]
      def liftK[F[_, _], A, B](fab: F[A, B]): FreeArrowChoice[F, A, B] = FreeArrowChoice.lift(fab)
    }
  }
}


