package com.adrielc.arrow.free

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.free.FreeArrowLike.ArrowK
import com.adrielc.arrow.free.FreeArrowLike.ArrowK.ArrowChoiceK




/**
 *
 * Method Enrichment class for Free structures [[FA]]
 *
 * Methods are derived from an implicit [[ArrowK]] instance for [[FA]], which provides an [[cats.arrow.Arrow]]
 * for any [[F]]. Since the behavior of [[FreeArrowLoop.foldMap]] is dependent on the [[cats.arrow.Arrow]] instance
 * behavior, it is best to have methods/alias methods on some arrow to behave the same as their type class
 * counter parts
 *
 * @param AK
 * @tparam FA Free arrow structure
 * @tparam F Arrow algebra
 * @tparam A Input
 * @tparam B Output
 */
abstract class FreeArrowLike[FA[+f[_, _], a, b] <: FreeArrowLike[FA, f, a, b], +F[_ ,_], A, B](implicit AK: ArrowK[FA])
  extends Product with Serializable {
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

  /** Alias for [[andThen]] */
  final def >>>[C, FF[a, b] >: F[a, b]](fbc: FA[FF, B, C]): FA[FF, A, C] = andThen(fbc)

  /** Alias for [[compose]] */
  final def <<<[AA, FF[a, b] >: F[a, b]](fca: FA[FF, AA, A]): FA[FF, AA, B] = compose(fca)

  /** Alias for [[split]] */
  final def ***[C, D, FF[a, b] >: F[a, b]](fcd: FA[FF, C, D]): FA[FF, (A, C), (B, D)] = split(fcd)

  /** Alias for [[merge]] */
  final def &&&[C, FF[a, b] >: F[a, b]](fac: FA[FF, A, C]): FA[FF, A, (B, C)] = merge(fac)

  /** [[andThen]] on a lifted [[F]] value */
  final def >>^[C, FF[a, b] >: F[a, b]](f: FF[B, C]): FA[FF, A, C] = andThen(AK.lift(f))

  /** [[compose]] on a lifted [[F]] value */
  final def ^<<[AA, FF[a, b] >: F[a, b]](f: FF[AA, A]): FA[FF, AA, B] = compose(AK.lift(f))

  /** Select first if output is a tuple */
  final def _1[C](implicit ev: B <:< (C, Any)): FA[F, A, C] = rmap(_._1)

  /** Select second if output is a tuple */
  final def _2[C](implicit ev: B <:< (Any, C)): FA[F, A, C] = rmap(_._2)

  /** Return tuple with input first and output second  */
  final def in_1: FA[F, A, (A, B)] = id merge self

  /** Return tuple with output first and input second  */
  final def in_2: FA[F, A, (B, A)] = merge(id)

  /** Return the untouched input [[A]] to this Arrow */
  final def in: FA[F, A, A] = in_2._2

  /** [[andThen]] with a dead end arrow, ignoring its output and returning this arrows output [[B]] */
  final def -|[FF[a, b] >: F[a, b]](fab: FA[FF, B, Unit]): FA[FF, A, B] = andThen(fab.in)

  /** Feed input [[A]] to two copies of this arrow and tuple the outputs */
  final def twice: FA[F, A, (B, B)] = merge(self)

  /** Duplicate/Fan out the output to a tuple */
  final def dup: FA[F, A, (B, B)] = rmap(o => (o, o))

  /**
   * If this arrows output is type equivalent to the input, then feed the output to this arrows input n times
   * [[andThen]] is Stack-safe when compiling the [[FA]] to some target arrow, but if the targets arrow
   * implementation has a stack-unsafe [[cats.arrow.Arrow.andThen]] implementation, running the interpretation
   * may blow the stack
   *
   * */
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
}


