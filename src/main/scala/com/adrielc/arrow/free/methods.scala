package com.adrielc.arrow.free

import cats.Monoid
import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.data.ConstP
import com.adrielc.arrow.{ArrowPlus, ~~>}

object methods {

  /**
   *
   * Method Enrichment class for Free structure [[Free]]
   *
   * Methods are derived from an implicit [[ArrowF]] instance for [[Free]], which provides an [[cats.arrow.Arrow]]
   * for any [[F]]. Since the behavior of [[FreeArrow.foldMap]] is dependent on the [[cats.arrow.Arrow]] instance
   * behavior, it is best to have methods/alias methods on some arrow to behave the same as the corresponding type class methods
   *
   * @tparam Free Free arrow structure
   * @tparam F Arrow algebra
   */
  abstract class FreeArrowLike[Free[+f[_, _], a, b] <: FreeArrowLike[Free, Ar, f, a, b], -Ar[f[_, _]] <: Arrow[f], +F[_ ,_], A, B](
    implicit AK: ArrowF.Aux[Free, Ar])
    extends Product with Serializable {
    self: Free[F, A, B] =>
    import AK._

    /** Interprets/Runs the sequence of operations using the semantics of `Arrow[G]` */
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Ar[G]): G[A, B]

    def analyze[M: Monoid](fm: F ~~> λ[(α, β) => M]): M =
      foldMap(new (F ~~> ConstP[M, ?, ?]) {
        def apply[C, D](f: F[C, D]): ConstP[M, C, D] = ConstP(fm(f))
      }).getConst

    def compile[G[_, _]](fg: F ~~> G): Free[G, A, B] =
      foldMap(new (F ~~> Free[G, ?, ?]) {
        def apply[X, Y](f: F[X, Y]): Free[G, X, Y] = AK.lift(fg(f))
      })

    @inline final def andThen[C, FF[a, b] >: F[a, b]](fbc: Free[FF, B, C]): Free[FF, A, C] = arrow.andThen(self, fbc)

    @inline final def split[C, D, FF[a, b] >: F[a, b]](fcd: Free[FF, C, D]): Free[FF, (A, C), (B, D)] = arrow.split(self, fcd)

    @inline final def merge[C, FF[a, b] >: F[a, b]](fac: Free[FF, A, C]): Free[FF, A, (B, C)] = arrow.merge(self, fac)

    @inline final def compose[C, FF[a, b] >: F[a, b]](fca: Free[FF, C, A]): Free[FF, C, B] = arrow.compose(self, fca)

    @inline final def rmap[C](f: B => C): Free[F, A, C] = arrow.rmap(self)(f)

    @inline final def lmap[C](f: C => A): Free[F, C, B] = arrow.lmap(self)(f)

    @inline final def first[C]: Free[F, (A, C), (B, C)] = arrow.first(self)

    @inline final def second[C]: Free[F, (C, A), (C, B)] = arrow.second(self)

    /** Alias for [[andThen]] */
    final def >>>[C, FF[a, b] >: F[a, b]](fbc: Free[FF, B, C]): Free[FF, A, C] = andThen(fbc)

    /** [[andThen]] on a lifted [[F]] value */
    final def >>>[C, FF[a, b] >: F[a, b]](f: FF[B, C]): Free[FF, A, C] = andThen(AK.lift(f))

    /** [[andThen]] on a lifted function */
    final def >>>[C](f: B => C): Free[F, A, C] = andThen(arrow.lift(f))

    /** Alias for [[compose]] */
    final def <<<[AA, FF[a, b] >: F[a, b]](fca: Free[FF, AA, A]): Free[FF, AA, B] = compose(fca)

    /** [[compose]] on a lifted [[F]] value */
    final def <<<[AA, FF[a, b] >: F[a, b]](f: FF[AA, A]): Free[FF, AA, B] = compose(AK.lift(f))

    /** [[compose]] with a lifted function */
    final def <<<[AA, FF[a, b] >: F[a, b]](f: AA => A): Free[FF, AA, B] = compose(arrow.lift(f))

    /** Alias for [[split]] */
    final def ***[C, D, FF[a, b] >: F[a, b]](fcd: Free[FF, C, D]): Free[FF, (A, C), (B, D)] = split(fcd)

    /** Alias for [[merge]] */
    final def &&&[C, FF[a, b] >: F[a, b]](fac: Free[FF, A, C]): Free[FF, A, (B, C)] = merge(fac)

    /** Select first if output is a tuple */
    final def _1[C](implicit ev: B <:< (C, Any)): Free[F, A, C] = rmap(_._1)

    /** Select second if output is a tuple */
    final def _2[C](implicit ev: B <:< (Any, C)): Free[F, A, C] = rmap(_._2)

    /** Return a tuple with output [[B]] first and input [[A]] second  */
    final def `|*>`: Free[F, A, (B, A)] = merge(id)

    /** Return a tuple with input [[A]] first and output [[B]] second  */
    final def `|>*`: Free[F, A, (A, B)] = id merge self

    /** Discard the output [[B]] and Return the input [[A]] */
    final def `*|>`: Free[F, A, A] = |*>._2

    /** [[andThen]] with a dead end arrow, ignoring its output and returning this arrows output [[B]] */
    final def -|>[FF[a, b] >: F[a, b]](fab: Free[FF, B, Unit]): Free[FF, A, B] = andThen(fab.*|>)

    /** Feed input [[A]] to two copies of this arrow and tuple the outputs */
    final def twice: Free[F, A, (B, B)] = merge(self)

    /** Duplicate/Fan out the output to a tuple */
    final def dup: Free[F, A, (B, B)] = rmap(o => (o, o))

    /**
     * If this arrows output is type equivalent to the input, then feed the output to this arrows input n times
     * [[andThen]] is Stack-safe when compiling the [[Free]] to some target arrow, but if the targets arrow
     * implementation has a stack-unsafe [[cats.arrow.Arrow.andThen]] implementation, running the interpretation
     * may blow the stack
     *
     * */
    @inline def loopN(n: Int)(implicit ev: B =:= A): Free[F, A, B] = {
      val _ = ev
      val init = self.asInstanceOf[Free[F, B, B]]
      var g = init
      for (_ <- 1 until n) { g = g andThen init }
      g.asInstanceOf[Free[F, A, B]]
    }

    @inline private final def id[C]: Free[F, C, C] = arrow[F].id[C]
  }

  abstract class FreeArrowChoiceLike[Free[+f[_, _], a, b] <: FreeArrowChoiceLike[Free, Ar, f, a, b], -Ar[f[_, _]] <: ArrowChoice[f], +F[_ ,_], A, B](
    implicit AK: ArrowF.Aux[Free, Ar])
    extends FreeArrowLike[Free, Ar, F, A, B] {
    self: Free[F, A, B] =>
    import AK._

    @inline final def choose[FF[a, b] >: F[a, b], C, D](fcd: Free[FF, C, D]): Free[FF, Either[A, C], Either[B, D]] = arrow[FF].choose(self)(fcd)

    @inline final def choice[FF[a, b] >: F[a, b], C](fcb: Free[FF, C, B]): Free[FF, Either[A, C], B] = arrow.choice(self, fcb)

    @inline final def left[FF[a, b] >: F[a, b], C]: Free[FF, Either[A, C], Either[B, C]] = arrow.left(self)

    @inline final def right[FF[a, b] >: F[a, b], C]: Free[FF, Either[C, A], Either[C, B]] = arrow.right(self)

    final def |||[FF[a, b] >: F[a, b], C](fcb: Free[FF, C, B]): Free[FF, Either[A, C], B] = choice(fcb)

    final def +++[FF[a, b] >: F[a, b], C, D](fcd: Free[FF, C, D]): Free[FF, Either[A, C], Either[B, D]] = choose(fcd)
  }

  abstract class FreeArrowPlusLike[Free[+f[_, _], a, b] <: FreeArrowPlusLike[Free, Ar, f, a, b], -Ar[f[_, _]] <: ArrowPlus[f], +F[_ ,_], A, B](
    implicit AK: ArrowF.Aux[Free, Ar])
    extends FreeArrowLike[Free, Ar, F, A, B] {
    self: Free[F, A, B] =>
    import AK._

    @inline final def plus[FF[a, b] >: F[a, b]](fcb: Free[FF, A, B]): Free[FF, A, B] = arrow.plus(self, fcb)

    @inline final def <+>[FF[a, b] >: F[a, b]](fcb: Free[FF, A, B]): Free[FF, A, B] = plus(fcb)
  }

  /** Universally quantified arrow for all Free Arrow like structures of shape [[Free]] */
  private[free] trait ArrowF[Free[_[_, _], _, _]] {
    type Arr[f[_, _]] <: Arrow[f]
    implicit def constArr[M: Monoid]: Arr[ConstP[M, ?, ?]]
    implicit def arrow[F[_, _]]: Arr[Free[F, ?, ?]]
    @inline def lift[F[_, _], A, B](fab: F[A, B]): Free[F, A, B]
    @inline def arr[A, B](fab: A => B): Free[Nothing, A, B] = arrow[Nothing].lift(fab)
  }

  private[free] object ArrowF {
    type Aux[Free[_[_, _], _, _], A[_[_, _]]] = ArrowF[Free] { type Arr[f[_, _]] = A[f] }
    def apply[Free[_[_, _], _, _]](implicit AK: ArrowF[Free]): ArrowF[Free] = AK
    implicit val freeArrowChoiceArrowChoiceK: ArrowF.Aux[FreeArrowChoice, ArrowChoice] = new ArrowF[FreeArrowChoice] {
      override type Arr[f[_, _]] = ArrowChoice[f]
      def constArr[M: Monoid]: Arr[ConstP[M, ?, ?]] = ConstP.constArrow
      def arrow[F[_, _]]: Arr[FreeArrowChoice[F, ?, ?]] = FAC.freeArrArrowChoice
      def lift[F[_, _], A, B](fab: F[A, B]): FreeArrowChoice[F, A, B] = FAC.lift(fab)
    }
    implicit val freeArrowArrowK: ArrowF.Aux[FreeArrow, Arrow] = new ArrowF[FreeArrow] {
      override type Arr[f[_, _]] = Arrow[f]
      def constArr[M: Monoid]: Arr[ConstP[M, ?, ?]] = ConstP.constArrow
      def arrow[F[_, _]]: Arr[FreeArrow[F, ?, ?]] = FA.freeArrArrow
      def lift[F[_, _], A, B](fab: F[A, B]): FreeArrow[F, A, B] = FA.lift(fab)
    }
    implicit val freeArrowPlusArrowK: ArrowF.Aux[FreeArrowPlus, ArrowPlus] = new ArrowF[FreeArrowPlus] {
      override type Arr[f[_, _]] = ArrowPlus[f]
      def constArr[M: Monoid]: Arr[ConstP[M, ?, ?]] = ConstP.constArrow
      def arrow[F[_, _]]: Arr[FreeArrowPlus[F, ?, ?]] = FAP.freeArrArrowPlus
      def lift[F[_, _], A, B](fab: F[A, B]): FreeArrowPlus[F, A, B] = FAP.lift(fab)
    }
  }
}
