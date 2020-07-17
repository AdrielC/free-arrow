package com.adrielc.arrow
package free

import cats.{Eval, Monoid}
import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.arrow.{ArrowChoicePlus, ArrowPlus, ArrowZero, ~>|, ~~>}
import cats.syntax.{either, flatMap}, either._, flatMap._
import com.adrielc.arrow.data.ConstA

/** Free Arrow
 *
 * Free construction of an Arrow for any context [[F]] with interpretation requirements of [[R]]
 *
 * @tparam R The capabilities required to interpret/fold this free arrow into [[F]]
 *           These capabilities adjust based on the methods used to create/compose
 *           the free arrow.
 *
 *           Must be a subtype of [[Arrow]] and supertype of [[ArrowChoicePlus]] since those
 *           are the currently supported typeclasses
 * @tparam F The underlying arrow context. Any type of kind (* -> * -> *) e.g. `AST[In, Out]` can be
 *           be composed together using methods from [[Arrow]] to [[ArrowChoicePlus]] without requiring
 *           an instance of the desired type class
 */
sealed trait FreeArrow[-R[f[_, _]] <: Arrow[f], +F[_, _], A, B] {
  self =>
  import FreeArrow._

  def foldMap[G[_, _]](fg: F ~~> G)(implicit A: R[G]): G[A, B]

  final def analyze[M: Monoid](m: F ~>| M): M =
    foldMap(new (F ~~> ConstA[M, ?, ?]) {
      def apply[C, D](f: F[C, D]): ConstA[M, C, D] = ConstA(m.apply(f))
    })(ConstA.constArrow[M](Monoid[M]).asInstanceOf[R[ConstA[M, ?, ?]]]).getConst

  /**
   * Modify the functor context `F` using transformation `fg`.
   *
   * This is effectively compiling your free arrow into another
   * language by applying the given `fg` each [[F]]
   *
   * If your binatural transformation is effectful, be careful. These
   * effects will be applied by `compile`.
   */
  final def compile[Ar[f[_, _]] >: ACP[f] <: R[f], G[_, _]](
    fg: F ~~> G
  ): FreeArrow[Ar, G, A, B] =
    foldMap(fg.andThen[FreeArrow[Ar, G, ?, ?]](lift))(arrowInstance[Ar, G])

  final def optimize[M: Monoid, Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b]](
    inspect: F ~>| M,
    optimize: |~>[Ar, FF, M]
  ): FreeArrow[Ar, FF, A, B] =
    foldMap[FreeArrow[Ar, FF, ?, ?]](FunctionA.id[F].withEnv(analyze[M](inspect)).andThen(optimize))(arrowInstance[Ar, FF])

  /**
   * Fuses any pure functions if possible, otherwise wraps the arrows in [[AndThen]]
   */
  def andThen[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C](
    fbc: FreeArrow[Ar, FF, B, C]
  ): FreeArrow[Ar, FF, A, C] = self match {

    case Fn(f) => fbc match {
      case Fn(g) => Fn(f andThen g)
      case a: AndThen[Ar, FF, B, b, C] => AndThen(andThen(a.begin), a.end)
      case _ => AndThen(self, fbc)
    }

    case AndThen(begin, end) => AndThen(begin, end andThen fbc)

    case _ => AndThen(self, fbc)
  }

  /**
   * Merges pure functions if possible, otherwise wraps the arrows in [[Merge]]
   */
  final def merge[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C](
    fac: FreeArrow[Ar, FF, A, C]
  ): FreeArrow[Ar, FF, A, (B, C)] =
    (self, fac) match {
      case (Fn(f), Fn(g)) => fn(a => (f(a), g(a)))
      case _ => Merge(self, fac)
    }

  /** [[First]], equivalent to [[cats.arrow.Strong.first]] */
  final def first[C]: FreeArrow[R, F, (A, C), (B, C)] = First(self)

  /** [[Second]], equivalent to [[cats.arrow.Strong.second]] */
  final def second[C]: FreeArrow[R, F, (C, A), (C, B)] = Second(self)

  /** [[Left]] */
  final def left[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C](implicit L: |||@[Ar])
  : FreeArrow[L.Lub, FF, Either[A, C], Either[B, C]] = Left(self)

  /** [[Right]] */
  final def right[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C](implicit L: |||@[Ar])
  : FreeArrow[L.Lub, FF, Either[C, A], Either[C, B]] = Right(self)

  /** [[Choice]] */
  final def choice[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C](
    fcb: FreeArrow[Ar, FF, C, B]
  )(implicit L: |||@[Ar]): FreeArrow[L.Lub, FF, Either[A, C], B] = Choice(self, fcb)

  /** [[Choose]] */
  final def choose[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C, D](
    fcd: FreeArrow[Ar, FF, C, D]
  )(implicit L: |||@[Ar]): FreeArrow[L.Lub, FF, Either[A, C], Either[B, D]] = Choose(self, fcd)

  /** [[Split]] */
  final def split[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C, D](
    fcd: FreeArrow[Ar, FF, C, D]
  ): FreeArrow[Ar, FF, (A, C), (B, D)] = Split(self, fcd)

  /** [[Plus]] */
  final def plus[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b]](
    fcb: FreeArrow[Ar, FF, A, B]
  )(implicit L: <+>@[Ar]): FreeArrow[L.Lub, FF, A, B] = Plus(self, fcb)


  /**
   * Aliases / Utility methods
   */

  final def compose[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C]
  (fca: FreeArrow[Ar, FF, C, A]): FreeArrow[Ar, FF, C, B] = fca andThen self

  /** Alias for [[andThen]] */
  def >>>[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C]
  (fbc: FreeArrow[Ar, FF, B, C]): FreeArrow[Ar, FF, A, C] = self.andThen(fbc)

  /** Alias for [[compose]] */
  def <<<[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C]
  (fca: FreeArrow[Ar, FF, C, A]): FreeArrow[Ar, FF, C, B] = self compose fca

  /** [[andThen]] on lifted function */
  def >>^[C](f: B => C): FreeArrow[R, F, A, C] = andThen(fn(f))

  /** [[compose]] with a lifted function */
  def <<^[C](f: C => A): FreeArrow[R, F, C, B] = compose(fn(f))

  /** Alias for [[split]] */
  def ***[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C, D]
  (fcd: FreeArrow[Ar, FF, C, D]): FreeArrow[Ar, FF, (A, C), (B, D)] = self.split(fcd)

  /** Alias for [[merge]] */
  def &&&[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b], C]
  (fac: FreeArrow[Ar, FF, A, C]): FreeArrow[Ar, FF, A, (B, C)] = self.merge(fac)

  /** Alias for [[choose]] */
  final def +++[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C, D]
  (fcd: FreeArrow[Ar, FF, C, D])(implicit L: |||@[Ar]): FreeArrow[L.Lub, FF, Either[A, C], Either[B, D]] = self.choose(fcd)

  /** Alias for [[choice]] */
  final def |||[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b], C]
  (fcb: FreeArrow[Ar, FF, C, B])(implicit L: |||@[Ar]): FreeArrow[L.Lub, FF, Either[A, C], B] = self.choice(fcb)

  /** Alias for [[plus]] */
  final def <+>[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b]]
  (fcb: FreeArrow[Ar, FF, A, B])(implicit L: <+>@[Ar]): FreeArrow[L.Lub, FF, A, B] = self.plus(fcb)

  /** test condition [[B]], Right == true */
  def test(implicit ev: B =:= Boolean): FreeArrow[R, F, A, Either[A, A]] =
    self.|>* >>^ (ba => if(ba._1) ba._2.asRight else ba._2.asLeft)

  def and[Ar[f[_, _]] >: ACP[f] <: R[f] : |||@, FF[a, b] >: F[a, b]]
  (fab: FreeArrow[Ar, FF, A, B])(implicit L: |&|@[Ar]): FreeArrow[L.Lub, FF, A, Either[B, B]] = arrowInstance[Ar, FF].and(self, fab)

  /** Alias for [[and]] */
  def |&|[Ar[f[_, _]] >: ACP[f] <: R[f], FF[a, b] >: F[a, b]]
  (fab: FreeArrow[Ar, FF, A, B])(implicit L: |&|@[Ar]): FreeArrow[L.Lub, FF, A, Either[B, B]] = and(fab)

  /** Select first if output is a tuple */
  def _1[C](implicit ev: B <:< (C, Any)): FreeArrow[R, F, A, C] = >>^(_._1)

  /** Select second if output is a tuple */
  def _2[C](implicit ev: B <:< (Any, C)): FreeArrow[R, F, A, C] = >>^(_._2)

  /** Return a tuple with output [[B]] first and input [[A]] second  */
  def `|>*`: FreeArrow[R, F, A, (B, A)] = self.merge(id)

  /** Return a tuple with input [[A]] first and output [[B]] second  */
  def `|*>`: FreeArrow[R, F, A, (A, B)] = id.merge(self)

  /** Feed input [[A]] to two copies of this arrow and tuple the outputs */
  def `|>>`: FreeArrow[R, F, A, (B, B)] = self.merge(self)

  /** Dead end. Discard the output [[B]] and Return the input [[A]] */
  def `|*`: FreeArrow[R, F, A, A] = self.|>*._2

  /** Appends a dead end arrow */
  def `>|`[Ar[f[_, _]] <: R[f], FF[a, b] >: F[a, b]]
  (deadEnd: FreeArrow[Ar, FF, B, Unit]): FreeArrow[Ar, FF, A, B] = self.andThen(deadEnd.|*)


  /**
   * If this arrows output is type equivalent to the input, then feed the output to this arrows input n times
   * [[andThen]] is Stack-safe when compiling the [[FreeArrow]] to some target arrow, but if the targets arrow
   * implementation has a stack-unsafe [[cats.arrow.Arrow.andThen]] implementation, running the interpretation
   * may blow the stack
   *
   * */
  @inline def loopN(n: Int)(implicit ev: B =:= A): FreeArrow[R, F, A, B] = {
    val _ = ev
    val init = self.asInstanceOf[FreeArrow[R, F, B, B]]
    var g = init
    for (_ <- 1 until n) { g = g.andThen(init) }
    g.asInstanceOf[FreeArrow[R, F, A, B]]
  }
}

object FreeArrow {

  /** Lift an algebra [[F]] into [[FA]] */
  @inline def lift[F[_, _]]: F ~~> FA[F, ?, ?] = new (F ~~> FA[F, ?, ?]) { def apply[A, B](f: F[A, B]): FA[F, A, B] = Lift(f) }

  /** Lift a pure function into [[FA]]. Can be composed with any arrow context */
  @inline final def id[A]: A >>> A = Id()

  @inline def fn[A, B](f: A => B): A >>> B = Fn(cats.data.AndThen(f))

  @inline def zeroArrow[A, B] : A ~@~ B = Zero()

  @inline def justLeft[A, B]  : B ^|- A = Z.justLeft

  @inline def justRight[A, B] : A -|^ B = Z.justRight

  val fork: FA[Nothing, Boolean, Either[Unit, Unit]] = FreeArrow.fn((b: Boolean) => if(b) ().asRight else ().asLeft)

  implicit def arrowInstance[Arr[f[_, _]] >: ArrowChoicePlus[f] <: Arrow[f], F[_, _]]
  : ArrowChoicePlus[FreeArrow[Arr, F, ?, ?]] with Arr[FreeArrow[Arr, F, ?, ?]] =
    new ArrowChoicePlus[FreeArrow[Arr, F, ?, ?]] {
      def compose[A, B, C](f: FreeArrow[Arr, F, B, C], g: FreeArrow[Arr, F, A, B]): FreeArrow[Arr, F, A, C] = g andThen f
      def lift[A, B](f: A => B): FreeArrow[Arr, F, A, B] = FreeArrow.fn(f)
      def first[A, B, C](fa: FreeArrow[Arr, F, A, B]): FreeArrow[Arr, F, (A, C), (B, C)] = fa.first
      def plus[A, B](f: FreeArrow[Arr, F, A, B], g: FreeArrow[Arr, F, A, B]): FreeArrow[Arr, F, A, B] =
        f.plus(g).asInstanceOf[FreeArrow[Arr, F, A, B]]
      def zeroArrow[B, C]: FreeArrow[Arr, F, B, C] =
        FreeArrow.zeroArrow[B, C].asInstanceOf[FreeArrow[Arr, F, B, C]]
      def choose[A, B, C, D](f: FreeArrow[Arr, F, A, C])(g: FreeArrow[Arr, F, B, D]): FreeArrow[Arr, F, Either[A, B], Either[C, D]] =
        (f +++ g).asInstanceOf[FreeArrow[Arr, F, Either[A, B], Either[C, D]]]
      override def second[A, B, C](fa: FreeArrow[Arr, F, A, B]): FreeArrow[Arr, F, (C, A), (C, B)] = fa.second
      override def rmap[A, B, C](fab: FreeArrow[Arr, F, A, B])(f: B => C): FreeArrow[Arr, F, A, C] = fab.>>^(f)
      override def lmap[A, B, C](fab: FreeArrow[Arr, F, A, B])(f: C => A): FreeArrow[Arr, F, C, B] = fab.<<^(f)
      override def id[A]: FreeArrow[Arr, F, A, A] = FreeArrow.id
      override def split[A, B, C, D](f: FreeArrow[Arr, F, A, B], g: FreeArrow[Arr, F, C, D]): FreeArrow[Arr, F, (A, C), (B, D)] = f split g
      override def merge[A, B, C](f: FreeArrow[Arr, F, A, B], g: FreeArrow[Arr, F, A, C]): FreeArrow[Arr, F, A, (B, C)] = f merge g
      override def choice[A, B, C](f: FreeArrow[Arr, F, A, C], g: FreeArrow[Arr, F, B, C]): FreeArrow[Arr, F, Either[A, B], C] =
        (f choice g).asInstanceOf[FreeArrow[Arr, F, Either[A, B], C]]
    }

  /**
   * This is not functionally necessary, since {{{ FreeA.fn(identity) }}} does the same thing,
   * but encoding it into the GADT comes in handy when introspecting the [[FreeArrow]] structure since
   * it can be distinguished from other anonymous functions.
   */
  final private[free] case class Id[A]() extends FreeArrow[Arrow, Nothing, A, A] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, A] = A.id
  }
  final private[free] case class Fn[A, B](f: A => B) extends FreeArrow[Arrow, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(f)
  }
  final private[free] case class Lift[F[_, _], A, B](fab: A F B) extends FreeArrow[Arrow, F, A, B] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arrow[G]): G[A, B] = fg(fab)
  }
  final private[free] case class AndThen[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    begin: FreeArrow[Arr, F, A, B],
    end: FreeArrow[Arr, F, B, C]
  ) extends FreeArrow[Arr, F, A, C] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[A, C] = {
      type EvalG[X, Y] = Eval[G[X, Y]]
      lazy val lazyAnd = new (FreeArrow[Arr, F, ?, ?] ~~> EvalG) {
        def apply[D, E](f: FreeArrow[Arr, F, D, E]): EvalG[D, E] = f match {
          case a: AndThen[Arr, F, d, b, e] =>

            for {
              b <- Eval.later(apply(a.begin)).flatten
              e <- apply(a.end)
            } yield A.andThen(b, e)

          case _ => Eval.now(f.foldMap(fk))
        }
      }

      val eval = for {
        e <- lazyAnd(begin)
        b <- Eval.later(lazyAnd(end)).flatten
      } yield A.andThen(e, b)
      eval.value
    }
  }
  final private[free] case class Merge[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _first: FreeArrow[Arr, F, A, B],
    _second: FreeArrow[Arr, F, A, C]
  ) extends FreeArrow[Arr, F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[A, (B, C)] =
      A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private[free] case class Split[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C, D](
    _first: FreeArrow[Arr, F, A, B],
    _second: FreeArrow[Arr, F, C, D]
  ) extends FreeArrow[Arr, F, (A, C), (B, D)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(A, C), (B, D)] =
      A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private[free] case class First[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _first: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[Arr, F, (A, C), (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(A, C), (B, C)] =
      A.first(_first.foldMap(fk))
  }
  final private[free] case class Second[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _second: FreeArrow[Arr, F, A, B]) extends FreeArrow[Arr, F, (C, A), (C, B)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(C, A), (C, B)] =
      A.second(_second.foldMap(fk))
  }
  final private[free] case class Choose[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C, D](
    _left: FreeArrow[Arr, F, A, B],
    _right: FreeArrow[Arr, F, C, D]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], Either[B, D]] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], Either[B, D]] =
      A.choose(_left.foldMap(fg))(_right.foldMap(fg))
  }
  final private[free] case class Left[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _left: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], Either[B, C]] {

    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], Either[B, C]] =
      A.left(_left.foldMap(fg))
  }
  final private[free] case class Right[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _right: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[C, A], Either[C, B]] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[C, A], Either[C, B]] =
      A.right(_right.foldMap(fg))
  }
  final private[free] case class Choice[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B, C](
    _left: FreeArrow[Arr, F, A, B],
    _right: FreeArrow[Arr, F, C, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], B] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], B] =
      A.choice(_left.foldMap(fg), _right.foldMap(fg))
  }
  final private[free] case class Zero[A, B]() extends FreeArrow[ArrowZero, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: ArrowZero[G]): G[A, B] =
      A.zeroArrow
  }
  final private[free] case class Plus[Arr[f[_, _]] <: Arrow[f], F[_, _], A, B](
    f: FreeArrow[Arr, F, A, B],
    g: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowPlus[α]], F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G] with ArrowPlus[G]): G[A, B] =
      A.plus(f.foldMap(fk), g.foldMap(fk))
  }

  private val Z = arrowInstance[ArrowChoiceZero, Nothing]
}
