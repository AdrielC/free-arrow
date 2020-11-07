package com.adrielc.quivr
package free

import cats.{Applicative, ContravariantMonoidal, Eval, Monad, Monoid, MonoidK, SemigroupK}
import cats.arrow.{Arrow, ArrowChoice, Category, Compose}
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.syntax.all._
import com.adrielc.quivr.data.{BiConst, BiEitherK, EnvA}
import com.adrielc.quivr.free.Lub.{<+>@, |&|@, |||@}

/** Free Arrow
 *
 * Free construction of an Arrow for any context [[Flow]] with interpretation requirements of [[R]]
 *
 * @tparam R    The capabilities required to interpret/fold this free arrow into [[Flow]]
 *
 *              These capabilities adjust based on the methods used to create/compose
 *              the free arrow.
 *
 *              Must be a subtype of [[FA]] and supertype of [[ArrowChoicePlus]] since those
 *              are the currently supported typeclasses
 * @tparam Flow The underlying arrow context. Any type of kind (* -> * -> *) e.g. `AST[In, Out]` can be
 *              be composed together using methods from [[R]] to [[ArrowChoicePlus]] without requiring
 *              an instance of the desired type class
 */
sealed abstract class FreeArrow[-R[f[_, _]] <: Arrow[f], +Flow[_, _], In, Out] {
  self =>
  import FreeArrow._

  /**
   * Evaluate this free arrow to a [[G]] using [[G]]s behavior for
   * the Arrow type [[R]]
   */
  def foldMap[G[_, _]](fg: Flow ~~> G)(implicit A: R[G]): G[In, Out]

  def fold[FF[a, b] >: Flow[a, b]](implicit R: R[FF]): FF[In, Out] =
    foldMap(BiFunctionK.id[FF])


  /**
   * Modify the arrow context `Flow` using transformation `fg`.
   *
   * This is effectively compiling your free arrow into another
   * language by applying the given `fg` each [[Flow]]
   *
   * If your binatural transformation is effectful, be careful. These
   * effects will be applied by `compile`.
   */
  final def compile[RR[f[_, _]] <: R[f], G[_, _]](fg: Flow ~~> G)(implicit R: RR[FreeArrow[RR, G, *, *]]): FreeArrow[RR, G, In, Out] = {
    (self: FreeArrow[RR, Flow, In, Out]).foldMap[FreeArrow[RR, G, *, *]](fg.andThen(BiFunctionK.lift(liftK)))(R)
  }

  final def flatCompile[RR[f[_, _]] <: R[f], G[_, _]](fg: Flow ~~> FreeArrow[RR, G, *, *])(implicit A: ->>[RR]): FreeArrow[RR, G, In, Out] =
    foldMap(fg)(A.freeArrowInstance[G])

  /** Fold this [[FreeArrow]] into a summary value using [[M]]s Monoidal behavior */
  final def analyze[RR[f[_, _]] <: R[f], M](
    m: Flow ~>| M
  )(implicit R: RR[BiConst[M, *, *]]): M =
    (self: FreeArrow[RR, Flow, In, Out]).foldMap(BiConst.liftK(m)).getConst

  /**
   *
   * Optimize/rewrite this FreeA by using a summary value [[M]].
   *
   * @param summarize  A binatural function from [[Flow]] to some monoid [[M]]
   *                 that will be folded over this structure to create a summary value.
   *                 This is lazily executed, and will only fold over the structure if
   *                 the [[M]] value is used in `optimize`
   * @param optimize A binatural function that can use the summary [[M]] to
   *                 rewrite [[Flow]] into a new [[FreeArrow]].
   *
   */
  final def optimize[
    RR[f[_, _]] <: R[f]: λ[α[f[_, _]] => RR[FreeArrow[α, FF, *, *]]],
    FF[a, b] >: Flow[a, b],
    M: Monoid: λ[α => RR[BiConst[α, *, *]]]
  ](summarize: FF ~>| M,
    optimize: |~>[M, RR, FF]
  ): FreeArrow[RR, FF, In, Out] =
    foldMap(EnvA(analyze[RR, M](summarize)).andThen(optimize))

  final def summarize[
    RR[f[_, _]] <: R[f]: ->>,
    M: Monoid: λ[α => RR[BiConst[α, *, *]]]
  ](summarize: Flow ~>| M): FreeArrow[R, Flow, In, (M, Out)] = {
    val summary = analyze[RR, M](summarize)
    self.rmap(summary -> _)
  }

  // Combinators

  /** Alias for [[andThen]] */
  def >>>[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fbc: FreeArrow[RR, FF, Out, C]
  ): FreeArrow[RR, FF, In, C] =
    self.andThen(fbc)

  /** Alias for [[compose]] */
  def <<<[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fca: FreeArrow[RR, FF, C, In]
  ): FreeArrow[RR, FF, C, Out] =
    self.compose(fca)

  /** Alias for [[rmap]] */
  def >^[C](f: Out => C): FreeArrow[R, Flow, In, C] =
    self.rmap(f)

  /** Alias for [[rmap]] */
  def >^[O1, O2, C](f: (O1, O2) => C)(implicit ev: Out =:= (O1, O2)): FreeArrow[R, Flow, In, C] =
    self.rmap(o => f.tupled(o))

  /** Alias for [[lmap]] */
  def <^[C](f: C => In): FreeArrow[R, Flow, C, Out] =
    self.lmap(f)

  /** [[liftK]] the argument into [[FreeArrow]] and [[andThen]]  */
  def >>^[FF[a, b] >: Flow[a, b], C](f: FF[Out, C]): FreeArrow[R, FF, In, C] =
    self >>> liftK(f)

  def <<^[FF[a, b] >: Flow[a, b], C](f: FF[C, In]): FreeArrow[R, FF, C, Out] =
    liftK(f) >>> self

  /** Alias for [[split]] */
  def ***[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C, D](
    fcd: FreeArrow[RR, FF, C, D]
  ): FreeArrow[RR, FF, (In, C), (Out, D)] =
    self.split(fcd)

  /** Alias for [[merge]] */
  def &&&[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fac: FreeArrow[RR, FF, In, C]
  ): FreeArrow[RR, FF, In, (Out, C)] =
    self.merge(fac)

  /** Alias for [[choose]] */
  final def +++[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C, D](
    fcd: FreeArrow[RR, FF, C, D]
  )(implicit L: |||@[RR]): FreeArrow[L.Lub, FF, Either[In, C], Either[Out, D]] =
    self.choose(fcd)

  /** Alias for [[choice]] */
  final def |||[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fcb: FreeArrow[RR, FF, C, Out]
  )(implicit L: |||@[RR]): FreeArrow[L.Lub, FF, Either[In, C], Out] =
    self.choice(fcb)

  /** Alias for [[plus]] */
  final def <+>[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b]](
    fcb: FreeArrow[RR, FF, In, Out]
  )(implicit L: <+>@[RR]): FreeArrow[L.Lub, FF, In, Out] =
    self.plus(fcb)

  /** Alias for [[and]] */
  def |&|[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fab: FreeArrow[RR, FF, In, C]
  )(implicit L: |&|@[RR]): FreeArrow[L.Lub, FF, In, Either[Out, C]] =
    self.and(fab)

  /** [[split]] wwith `fab` and then [[cats.Semigroup.combine]] the tupled [[Out]] */
  def ***|+|[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], A](
    fab: FreeArrow[RR, FF, A, Out]
  )(implicit S: Semigroup[Out]): FreeArrow[RR, FF, (In, A), Out] =
    self.split(fab).rmap((S.combine _).tupled)

  /** [[merge]] wwith `fab` and then [[cats.Semigroup.combine]] the tupled [[Out]] */
  def &&&|+|[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], A](
    fab: FreeArrow[RR, FF, In, Out]
  )(implicit S: Semigroup[Out]): FreeArrow[RR, FF, In, Out] =
    self.mergeWith(fab)(_ |+| _)

  /** Alias for [[andThen]] with [[plus]] */
  def >++[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fbc: FreeArrow[RR, FF, Out, C],
    fbcs: FreeArrow[RR, FF, Out, C]*
  )(implicit L: <+>@[RR]): FreeArrow[L.Lub, FF, In, C] = {
    val f = NonEmptyList(fbc, fbcs.toList)
    FreeArrow.plus(f).compose(self)
  }

  /** Select first if output is a tuple */
  def _1[C](implicit ev: Out <:< (C, Any)): FreeArrow[R, Flow, In, C] =
    self.rmap(_._1)

  /** Select second if output is a tuple */
  def _2[C](implicit ev: Out <:< (Any, C)): FreeArrow[R, Flow, In, C] =
    self.rmap(_._2)

  /** Return a tuple with output [[Out]] first and input [[In]] second  */
  def ->* : FreeArrow[R, Flow, In, (Out, In)] =
    self.merge(id)

  def >*^[C](f: (Out, In) => C): FreeArrow[R, Flow, In, C] =
    self.->*.rmap(f.tupled)

  /** Return a tuple with input [[In]] first and output [[Out]] second  */
  def -*> : FreeArrow[R, Flow, In, (In, Out)] =
    id.merge(self)

  /** Dead end. Discard the output [[Out]] and Return the input [[In]] */
  def -* : FreeArrow[R, Flow, In, In] =
    self.->*._2

  /** Feed input [[In]] to two copies of this arrow and tuple the outputs */
  def =>> : FreeArrow[R, Flow, In, (Out, Out)] =
    self.merge(self)

  /** duplicate the output [[Out]] */
  def ->> : FreeArrow[R, Flow, In, (Out, Out)] =
    self.rmap(o => (o, o))

  /** feed [[Out]] to a dead end arrow, ignoring its output and returning the [[Out]] */
  def >>|[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b]](
    deadEnd: FreeArrow[RR, FF, Out, Unit]
  ): FreeArrow[RR, FF, In, Out] =
    self.andThen(deadEnd.-*)

  /** feed [[Monoid.empty]] to the input of [[mergeR]], and thread it's output tupled right of [[Out]] */
  def >>/[RR[f[_, _]] >: AR[f] <: R[f], FF[a, b] >: Flow[a, b], I: Monoid, A](
    mergeR: FreeArrow[RR, FF, I, A]
  ): FreeArrow[RR, FF, In, (Out, A)] =
    lift((a: In) => (a, Monoid.empty[I])).andThen(self.split(mergeR))

  /** feed [[Monoid.empty]] to the input of [[mergeL]], and thread it's output tupled left of [[Out]] */
  def >>\[RR[f[_, _]] >: AR[f] <: R[f], FF[a, b] >: Flow[a, b], I: Monoid, A](
    mergeL: FreeArrow[RR, FF, I, A]
  ): FreeArrow[RR, FF, In, (A, Out)] =
    lift((a: In) => (Monoid.empty[I], a)).andThen(mergeL.split(self))


  /** feed [[Out]] to a dead end arrow, ignoring its output and returning the [[Out]] */
  def tapOut(tap: Out => Unit): FreeArrow[R, Flow, In, Out] =
    self >>| lift(tap)

  def tapIn(tap: In => Unit): FreeArrow[R, Flow, In, Out] =
    (lift(tap) &&& self)._2


  /** test condition [[Out]], Right == true */
  def test(implicit ev: Out =:= Boolean): FreeArrow[R, Flow, In, Either[In, In]] =
    self >*^ ((a, b) => if(a) b.asRight else b.asLeft)

  /**
   * If this arrows output is type equivalent to the input, then feed the output to this arrows input n times
   * [[andThen]] is Stack-safe when compiling the [[FreeArrow]] to some target arrow, but if the targets arrow
   * implementation has a stack-unsafe [[cats.arrow.Arrow.andThen]] implementation, running the interpretation
   * may blow the stack
   *
   * */
  def loopN(n: Int)(implicit ev: Out =:= In): FreeArrow[R, Flow, In, Out] = {
    val _ = ev
    val init = self.asInstanceOf[FreeArrow[R, Flow, Out, Out]]
    var g = init
    for (_ <- 1 until n) { g = g.andThen(init) }
    g.asInstanceOf[FreeArrow[R, Flow, In, Out]]
  }

  /**
   * Fuses any pure functions if possible, otherwise wraps the arrows in [[AndThen]]
   */
  def andThen[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fbc: FreeArrow[RR, FF, Out, C]
  ): FreeArrow[RR, FF, In, C] =
    self match {
      case AndThen(begin, end) => AndThen(begin, AndThen(end, fbc))
      case Lift(f) => fbc match {
        case Lift(g) => Lift(f andThen g)
        case _ => AndThen(self, fbc)
      }
      case _ => AndThen(self, fbc)
    }

  final def compose[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fca: FreeArrow[RR, FF, C, In]
  ): FreeArrow[RR, FF, C, Out] =
    fca.andThen(self)

  /** [[andThen]] on lifted function */
  def rmap[C](f: Out => C): FreeArrow[R, Flow, In, C] =
    self.andThen(lift(f))

  /** [[compose]] with a lifted function */
  def lmap[C](f: C => In): FreeArrow[R, Flow, C, Out] =
    self.compose(lift(f))

  final def merge[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fac: FreeArrow[RR, FF, In, C]
  ): FreeArrow[RR, FF, In, (Out, C)] =
    Merge(self, fac)

  final def mergeWith[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C, D](
    fac: FreeArrow[RR, FF, In, C]
  )(f: (Out, C) => D): FreeArrow[RR, FF, In, D] =
    (self &&& fac) >^ f

  /** [[First]], equivalent to [[cats.arrow.Strong.first]] */
  final def first[C]: FreeArrow[R, Flow, (In, C), (Out, C)] =
    First(self)

  /** [[Second]], equivalent to [[cats.arrow.Strong.second]] */
  final def second[C]: FreeArrow[R, Flow, (C, In), (C, Out)] =
    Second(self)

  /** [[Left]] */
  final def left[C](implicit L: |||@[R]): FreeArrow[L.Lub, Flow, Either[In, C], Either[Out, C]] =
    Left(self)

  def left[RR[f[_, _]] <: AC[f], FF[a, b] >: Flow[a, b], C, E, S](
    fbc: FreeArrow[RR, FF, E, C]
  )(implicit ev: Out <:< Either[E, S], L: |||@[RR with R]): FreeArrow[L.Lub, FF, In, Either[C, S]] =
    self >>> fbc.left[S].lmap(ev(_))

  /** [[Right]] */
  final def right[C](implicit L: |||@[R]): FreeArrow[L.Lub, Flow, Either[C, In], Either[C, Out]] =
    Right(self)

  def right[RR[f[_, _]] <: AC[f], FF[a, b] >: Flow[a, b], C, E, S](
    fbc: FreeArrow[RR, FF, E, C]
  )(implicit ev: Out <:< Either[S, E], L: |||@[RR with R]): FreeArrow[L.Lub, FF, In, Either[S, C]] =
    self >>> fbc.right[S].lmap(ev(_))

  /** [[Choice]] */
  final def choice[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fcb: FreeArrow[RR, FF, C, Out]
  )(implicit L: |||@[RR]): FreeArrow[L.Lub, FF, Either[In, C], Out] =
    Choice(self, fcb)

  /** [[Choose]] */
  final def choose[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C, D](
    fcd: FreeArrow[RR, FF, C, D]
  )(implicit L: |||@[RR]): FreeArrow[L.Lub, FF, Either[In, C], Either[Out, D]] =
    Choose(self, fcd)

  /** [[Split]] */
  final def split[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C, D](
    fcd: FreeArrow[RR, FF, C, D]
  ): FreeArrow[RR, FF, (In, C), (Out, D)] =
    Split(self, fcd)

  /** [[Plus]] */
  final def plus[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b]](
    fcb: FreeArrow[RR, FF, In, Out]
  )(implicit L: <+>@[RR]): FreeArrow[L.Lub, FF, In, Out] =
    Plus(self, fcb)

  def and[RR[f[_, _]] <: R[f], FF[a, b] >: Flow[a, b], C](
    fab: FreeArrow[RR, FF, In, C]
  )(implicit L: |&|@[RR]): FreeArrow[L.Lub, FF, In, Either[Out, C]] =
    And(self, fab)
}

object FreeArrow extends FreeArrowInstances {

  implicit class FreeArrowCompileOps[R[f[_, _]] <: AR[f], Flow[_, _], In, Out](private val fab: FreeArrow[R, Flow, In, Out]) extends AnyVal {


    /**
     * Embed context in arrow coproduct of [[Flow]] and [[G]]
     * [[Flow]] on the left
     */
    def inject[G[_, _]](implicit ev: Flow :<<: G, R: R ->>> G): FreeArrow[R, G, In, Out] =
      fab.compile(ev.inj)


    /** [[andThen]] with [[BiInjectK.inj]] */
    def >>>^[C, G[_, _]](
      fbc: FreeArrow[R, G, Out, C]
    )(implicit in: G :<<: Flow, R: R ->>> Flow): FreeArrow[R, Flow, In, C] =
      fab.andThen(fbc.inject[Flow])


    /**
     * Embed context in arrow coproduct of [[Flow]] and [[G]]
     * [[Flow]] on the left
     */
    def inl[G[_, _]](implicit A: R ->>> BiEitherK[Flow, G, *, *]): EitherFreeA[R, Flow, G, In, Out] =
      fab.compile[R, BiEitherK[Flow, G, *, *]](BiEitherK.leftK)

    /**
     * Embed context in arrow coproduct of [[Flow]] and [[G]]
     * [[Flow]] on the right
     */
    def inr[G[_, _]](implicit A: R ->>> BiEitherK[G, Flow, *, *]): EitherFreeA[R, G, Flow, In, Out] =
      fab.compile[R, BiEitherK[G, Flow, *, *]](BiEitherK.rightK)
  }

  @inline final def apply[A]: A >>> A = id

  @inline def apply[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = LiftK(fab)

  /** Lift a pure function into [[FA]]. Can be composed with any arrow context */
  @inline final def id[A]: A >>> A = Id()

  /** Lift a pure function into into [[FA]] */
  @inline def lift[A, B](f: A => B): A >>> B = Lift(cats.data.AndThen(f))

  @inline def lift2[A, B, C](f: (A, B) => C): (A, B) >>> C = Lift(cats.data.AndThen(f.tupled))

  @inline def lift3[A, B, C, D](f: (A, B, C) => D): (A, B, C) >>> D = Lift(cats.data.AndThen(f.tupled))

  /** Lift an [[F]] into [[FA]] */
  @inline def liftK[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = LiftK(fab)

  /** Lift an [[F]] into [[FA]] */
  implicit class FOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
    def unary_~ : FA[F, A, B] = liftK(fab)
  }

  @inline def zeroArrow[A, B]: A ~@~ B = Zero()

  @inline def justLeft[A, B]: B ^|- A = Z.justLeft

  @inline def justRight[A, B]: A -|^ B = Z.justRight

  /** Lift a plain value into into [[FA]] */
  @inline def const[A, B](value: B): A >>> B = Const(value)

  @inline def empty[A, B: Monoid] : A >>> B = Const(Monoid.empty[B])

  /** Always evaluate [[B]] */
  @inline def always[A, B](eval: => B): A >>> B = Always(Eval.always(eval))

  @inline def plus[R[f[_, _]] <: AR[f], F[_, _], A, B](f: FreeArrow[R, F, A, B], fs: FreeArrow[R, F, A, B]*)
                                                      (implicit L: <+>@[R]): FreeArrow[L.Lub, F, A, B] =
    plus(NonEmptyList(f, fs.toList))

  @inline def plus[R[f[_, _]] <: AR[f], F[_, _], A, B](nel: NonEmptyList[FreeArrow[R, F, A, B]])
                                                      (implicit L: <+>@[R]): FreeArrow[L.Lub, F, A, B] =
    PlusAll(nel)


  /**
   * This is not functionally necessary, since {{{ FreeA.fn(identity) }}} does the same thing,
   * but encoding it into the GADT comes in handy when introspecting the [[FreeArrow]] structure since
   * it can be distinguished from other anonymous functions.
   */
  final private case class Id[A]() extends FreeArrow[Arrow, Nothing, A, A] {

    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, A] = A.id
  }
  final private case class Lift[A, B](f: cats.data.AndThen[A, B]) extends FreeArrow[Arrow, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(f)
  }
  final private case class Const[A, B](value: B) extends FreeArrow[Arrow, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(_ => value)
  }
  final private case class Always[A, B](eval: Eval[B]) extends FreeArrow[Arrow, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(_ => eval.value)
  }
  final private case class LiftK[F[_, _], A, B](fab: F[A, B]) extends FreeArrow[Arrow, F, A, B] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arrow[G]): G[A, B] = fg(fab)
    override def toString: String = fab.toString
  }
  final private case class AndThen[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    begin: FreeArrow[Arr, F, A, B],
    end: FreeArrow[Arr, F, B, C]
  ) extends FreeArrow[Arr, F, A, C] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[A, C] = {
      type EvalG[X, Y] = Eval[G[X, Y]]
      lazy val lazyAnd = new (FreeArrow[Arr, F, *, *] ~~> EvalG) {
        def apply[D, E](f: FreeArrow[Arr, F, D, E]): EvalG[D, E] = f match {
          case a: AndThen[Arr, F, d, b, e] =>

            for {
              b <- Eval.later(apply(a.begin)).flatten
              e <- apply(a.end)
            } yield A.andThen(b, e)

          case _ => Eval.later(f.foldMap(fk))
        }
      }

      val eval = for {
        e <- lazyAnd(begin)
        b <- Eval.later(lazyAnd(end)).flatten
      } yield A.andThen(e, b)
      eval.value
    }
  }
  final private case class Merge[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _first: FreeArrow[Arr, F, A, B],
    _second: FreeArrow[Arr, F, A, C]
  ) extends FreeArrow[Arr, F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[A, (B, C)] =
      A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class Split[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C, D](
    _first: FreeArrow[Arr, F, A, B],
    _second: FreeArrow[Arr, F, C, D]
  ) extends FreeArrow[Arr, F, (A, C), (B, D)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(A, C), (B, D)] =
      A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class First[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _first: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[Arr, F, (A, C), (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(A, C), (B, C)] =
      A.first(_first.foldMap(fk))
  }
  final private case class Second[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _second: FreeArrow[Arr, F, A, B]) extends FreeArrow[Arr, F, (C, A), (C, B)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arr[G]): G[(C, A), (C, B)] =
      A.second(_second.foldMap(fk))
  }
  final private case class Choose[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C, D](
    _left: FreeArrow[Arr, F, A, B],
    _right: FreeArrow[Arr, F, C, D]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], Either[B, D]] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], Either[B, D]] =
      A.choose(_left.foldMap(fg))(_right.foldMap(fg))
  }
  final private case class Left[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _left: FreeArrow[Arr, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], Either[B, C]] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], Either[B, C]] =
      A.left(_left.foldMap(fg))
  }
  final private case class Right[R[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _right: FreeArrow[R, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => R[α] with ArrowChoice[α]], F, Either[C, A], Either[C, B]] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: R[G] with ArrowChoice[G]): G[Either[C, A], Either[C, B]] =
      A.right(_right.foldMap(fg))
  }
  final private[free] case class Choice[Arr[f[_, _]] <: AR[f], F[_, _], A, B, C](
    _left: FreeArrow[Arr, F, A, B],
    _right: FreeArrow[Arr, F, C, B]
  ) extends FreeArrow[λ[α[_, _] => Arr[α] with ArrowChoice[α]], F, Either[A, C], B] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: Arr[G] with ArrowChoice[G]): G[Either[A, C], B] =
      A.choice(_left.foldMap(fg), _right.foldMap(fg))
  }
  final private case class Zero[A, B]() extends FreeArrow[ArrowZero, Nothing, A, B] {
    def foldMap[G[_, _]](fg: Nothing ~~> G)(implicit A: ArrowZero[G]): G[A, B] =
      A.zeroArrow
  }
  final private case class Plus[R[f[_, _]] <: AR[f], +F[_, _], A, B](
    f: FreeArrow[R, F, A, B],
    g: FreeArrow[R, F, A, B]
  ) extends FreeArrow[λ[α[_, _] => R[α] with ArrowPlus[α]], F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: R[G] with ArrowPlus[G]): G[A, B] =
      A.plus(f.foldMap(fk), g.foldMap(fk))
  }
  final private case class And[R[f[_, _]] <: AR[f], F[_, _], A, B, C](
    f: FreeArrow[R, F, A, B],
    g: FreeArrow[R, F, A, C]
  ) extends FreeArrow[λ[α[_, _] => R[α] with ArrowChoicePlus[α]], F, A, Either[B, C]] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: R[G] with ArrowChoicePlus[G]): G[A, Either[B, C]] =
      A.and(f.foldMap(fk), g.foldMap(fk))
  }
  final private case class PlusAll[R[f[_, _]] <: AR[f], F[_, _], A, B](
    nel: NonEmptyList[FreeArrow[R, F, A, B]]
  ) extends FreeArrow[λ[α[_, _] => R[α] with ArrowPlus[α]], F, A, B] {
    def foldMap[G[_, _]](fg: F ~~> G)(implicit A: R[G] with ArrowPlus[G]): G[A, B] =
      nel.reduceLeftTo(_.foldMap(fg))((g, fab) => A.plus(g, fab.foldMap(fg)))
  }

  private val Z = freeArrowInstance[ACZ, Nothing]
}

private[free] trait FreeArrowInstances extends FreeArrowInstances0 {

  trait ->>[R[f[_, _]] <: AR[f]] {
    def freeArrowInstance[F[_, _]]: R[FreeArrow[R, F, *, *]]
  }
  object ->> {
    implicit val acp: ->>[ACP] = new ->>[ACP] { override def freeArrowInstance[F[_, _]]: ACP[FreeArrow[ACP, F, *, *]] = freeArrowACPInstance[F] }
    implicit val acz: ->>[ACZ] = new ->>[ACZ] { override def freeArrowInstance[F[_, _]]: ACZ[FreeArrow[ACZ, F, *, *]] = freeArrowACZInstance[F] }
    implicit val ac: ->>[AC] = new ->>[AC] { override def freeArrowInstance[F[_, _]]: AC[FreeArrow[AC, F, *, *]] = freeArrowACInstance[F] }
    implicit val ap: ->>[AP] = new ->>[AP] { override def freeArrowInstance[F[_, _]]: AP[FreeArrow[AP, F, *, *]] = freeArrowAPInstance[F] }
    implicit val aZ: ->>[AZ] = new ->>[AZ] { override def freeArrowInstance[F[_, _]]: AZ[FreeArrow[AZ, F, *, *]] = freeArrowAZInstance[F] }
    implicit val ar: ->>[AR] = new ->>[AR] { override def freeArrowInstance[F[_, _]]: AR[FreeArrow[AR, F, *, *]] = freeArrowARInstance[F] }
  }
  implicit def freeArrowInstance[R[f[_, _]] >: ACZ[f] <: AR[f], F[_, _]](implicit A: ->>[R]): R[FreeArrow[R, F, *, *]] = A.freeArrowInstance[F]
  def freeArrowACPInstance[F[_, _]]: ArrowChoicePlus[FreeArrow[ACP, F, *, *]]  = new FreeArrowArrowChoicePlus[ACP, F] { val L: Lub.Aux[ACP, ACP, ACP] = Lub.acp }
  def freeArrowACZInstance[F[_, _]]: ArrowChoiceZero[FreeArrow[ACZ, F, *, *]]  = new FreeArrowArrowChoiceZero[ACZ, F] { val L: Lub.Aux[ACZ, ACZ, ACZ] = Lub.acz }
  def freeArrowACInstance[F[_, _]]: ArrowChoice[FreeArrow[AC, F, *, *]]  = new FreeArrowArrowChoice[AC, F] { val L: Lub.Aux[AC, AC, AC] = Lub.ac }
  def freeArrowAPInstance[F[_, _]]: ArrowPlus[FreeArrow[AP, F, *, *]]  = new FreeArrowArrowPlus[AP, F] { val L: Lub.Aux[AP, AP, AP] = Lub.ap }
  def freeArrowAZInstance[F[_, _]]: ArrowZero[FreeArrow[AZ, F, *, *]]  = new FreeArrowArrowZero[AZ, F] { val L: Lub.Aux[AZ, AZ, AZ] = Lub.az }
  def freeArrowARInstance[F[_, _]]: Arrow[FreeArrow[AR, F, *, *]]  = new FreeArrowArrow[AR, F] {}


  implicit def applicativeNested[M[_]: Applicative, Arr[f[_, _]] <: AR[f], F[_, _], I]: Applicative[λ[α => FreeArrow[Arr, F, I, M[α]]]] =
    new Applicative[λ[α => FreeArrow[Arr, F, I, M[α]]]] {

      final def pure[A](x: A): FreeArrow[Arr, F, I, M[A]] = FA.const(x.pure[M])

      final def ap[A, B](ff: FreeArrow[Arr, F, I, M[A => B]])(fa: FreeArrow[Arr, F, I, M[A]]): FreeArrow[Arr, F, I, M[B]] =
        ff.mergeWith(fa)(_ ap _ )

      override def map[A, B](fa: FreeArrow[Arr, F, I, M[A]])(f: A => B): FreeArrow[Arr, F, I, M[B]] =
        fa.rmap(_.map(f))

      override def product[A, B](fa: FreeArrow[Arr, F, I, M[A]], fb: FreeArrow[Arr, F, I, M[B]]): FreeArrow[Arr, F, I, M[(A, B)]] =
        fa.mergeWith(fb)(_ product _)
    }

  implicit def semigroup[Arr[f[_, _]] <: AR[f], F[_, _], I, O: Semigroup]: Semigroup[FreeArrow[Arr, F, I, O]] =
    Semigroup.instance(_ &&&|+| _)

  implicit def monoid[Arr[f[_, _]] <: AR[f], F[_, _], I, O: Monoid]: Monoid[FreeArrow[Arr, F, I, O]] =
    Monoid.instance(FreeArrow.empty, _ &&&|+| _)

  implicit def semigroupK[Arr[f[_, _]] <: AR[f], F[_, _], I, M[_]: SemigroupK]: SemigroupK[λ[α => FreeArrow[Arr, F, I, M[α]]]] =
    new SemigroupK[λ[α => FreeArrow[Arr, F, I, M[α]]]] {
      def combineK[A](x: FreeArrow[Arr, F, I, M[A]], y: FreeArrow[Arr, F, I, M[A]]): FreeArrow[Arr, F, I, M[A]] = {
        implicit val s = SemigroupK[M].algebra[A]
        x &&&|+| y
      }
    }

  implicit def freeArrowMonoidK[Arr[f[_, _]] <: AR[f], F[_, _], I, M[_]: MonoidK]: MonoidK[λ[α => FreeArrow[Arr, F, I, M[α]]]] =
    new MonoidK[λ[α => FreeArrow[Arr, F, I, M[α]]]] {
      def empty[A]: FreeArrow[Arr, F, I, M[A]] =
        FA.empty(MonoidK[M].algebra)
      def combineK[A](x: FreeArrow[Arr, F, I, M[A]], y: FreeArrow[Arr, F, I, M[A]]): FreeArrow[Arr, F, I, M[A]] = {
        implicit val s = SemigroupK[M].algebra[A]
        x &&&|+| y
      }
    }
}


private[free] trait FreeArrowInstances0 {

  implicit def freeArrowContravariantMonoidal[Arr[f[_, _]] <: AR[f], F[_, _], O: Monoid]: ContravariantMonoidal[FreeArrow[Arr, F, *, O]] =
    new ContravariantMonoidal[FreeArrow[Arr, F, *, O]] {

      override def contramap[A, B](fa: FreeArrow[Arr, F, A, O])(f: B => A): FreeArrow[Arr, F, B, O] =
        fa.lmap(f)

      override def unit: FreeArrow[Arr, F, Unit, O] =
        FA.empty

      override def product[A, B](fa: FreeArrow[Arr, F, A, O], fb: FreeArrow[Arr, F, B, O]): FreeArrow[Arr, F, (A, B), O] =
        fa ***|+| fb
    }


  implicit def composedArrow[M[_]: Monad]: Arrow[λ[(α, β) => α >>> M[β]]] = new AR[λ[(α, β) => α >>> M[β]]] {

    import cats.implicits._

    def lift[A, B](f: A => B): A >>> M[B] = FreeArrow.lift(a => f(a).pure[M])

    def compose[A, B, C](f: B >>> M[C], g: A >>> M[B]): A >>> M[C] = {
      val ff = f.fold[Function1]
      val gg = g.fold[Function1]
      FreeArrow.lift(a => gg(a).flatMap(b => ff(b)))
    }

    def first[A, B, C](fa: A >>> M[B]): (A, C) >>> M[(B, C)] =
      fa.first[C] >^ ((m: M[B], c: C) => m.map(_ -> c))
  }
}

sealed private[free] trait FreeArrowArrowChoiceZero[R[f[_, _]] >: ACZ[f]  <: ACZ[f], F[_, _]]
  extends ArrowChoiceZero[FreeArrow[R, F, *, *]]
  with FreeArrowArrowChoicePlus[R, F]
  with FreeArrowArrowZero[R, F]

sealed private[free] trait FreeArrowArrowChoicePlus[R[f[_, _]] >: ACZ[f] <: ACP[f], F[_, _]] extends ArrowChoicePlus[FreeArrow[R, F, *, *]]
  with FreeArrowArrowChoice[R, F]
  with FreeArrowArrowPlus[R, F]

sealed private[free] trait FreeArrowArrowPlus[R[f[_, _]] >: ACZ[f]  <: AP[f], F[_, _]] extends ArrowPlus[FreeArrow[R, F, *, *]] with FreeArrowArrow[R, F] {

  implicit val L: Lub.Aux[R, R, R]

  override def plus[A, B](f: FreeArrow[R, F, A, B], g: FreeArrow[R, F, A, B]): FreeArrow[R, F, A, B] =
    f plus g
}

sealed private[free] trait FreeArrowArrowZero[R[f[_, _]] >: ACZ[f]  <: AZ[f], F[_, _]] extends ArrowZero[FreeArrow[R, F, *, *]] with FreeArrowArrowPlus[R, F] {

  override def zeroArrow[B, C]: FreeArrow[R, F, B, C] = FreeArrow.zeroArrow[B, C]
}


sealed private[free] trait FreeArrowArrowChoice[R[f[_, _]] >: ACZ[f]  <: AC[f], F[_, _]] extends ArrowChoice[FreeArrow[R, F, *, *]] with FreeArrowArrow[R, F] {

  implicit val L: Lub.Aux[R, R, R]

  override def choice[A, B, C](f: FreeArrow[R, F, A, C], g: FreeArrow[R, F, B, C]): FreeArrow[R, F, Either[A, B], C] =
    f ||| g

  def choose[A, B, C, D](f: FreeArrow[R, F, A, C])(g: FreeArrow[R, F, B, D]): FreeArrow[R, F, Either[A, B], Either[C, D]] =
    f +++ g

  override def left[A, B, C](fab: FreeArrow[R, F, A, B]): FreeArrow[R, F, Either[A, C], Either[B, C]] =
    fab.left[C]

  override def right[A, B, C](fab: FreeArrow[R, F, A, B]): FreeArrow[R, F, Either[C, A], Either[C, B]] =
    fab.right[C]
}

sealed private[free] trait FreeArrowArrow[R[f[_, _]] >: ACZ[f]  <: AR[f], F[_, _]] extends Arrow[FreeArrow[R, F, *, *]] with FreeArrowCategory[R, F] {
  override def lift[A, B](f: A => B): FreeArrow[R, F, A, B] = FreeArrow.lift(f)
  override def first[A, B, C](fa: FreeArrow[R, F, A, B]): FreeArrow[R, F, (A, C), (B, C)] = fa.first
  override def second[A, B, C](fa: FreeArrow[R, F, A, B]): FreeArrow[R, F, (C, A), (C, B)] = fa.second
  override def rmap[A, B, C](fab: FreeArrow[R, F, A, B])(f: B => C): FreeArrow[R, F, A, C] = fab.rmap(f)
  override def lmap[A, B, C](fab: FreeArrow[R, F, A, B])(f: C => A): FreeArrow[R, F, C, B] = fab.lmap(f)
}
sealed private[free] trait FreeArrowCategory[R[f[_, _]] <: AR[f], F[_, _]] extends Category[FreeArrow[R, F, *, *]] with FreeArrowCompose[R, F] {
  override def id[A]: FreeArrow[R, F, A, A] = FreeArrow.id
}
sealed private[free] trait FreeArrowCompose[R[f[_, _]] <: AR[f], F[_, _]] extends Compose[FreeArrow[R, F, *, *]] {
  override def compose[A, B, C](f: FreeArrow[R, F, B, C], g: FreeArrow[R, F, A, B]): FreeArrow[R, F, A, C] = f compose g
}


/** For unifying types between Arrows when mixing FreeA capabilities */
trait Lub[+F[f[_, _]], +G[f[_, _]]] {
  type Lub[f[_, _]] <: G[f] with F[f]
}
object Lub extends LubArrow0 {
  type Aux[+F[f[_, _]], +G[f[_, _]], O[_[_, _]]] = Lub[F, G] { type Lub[f[_, _]] = O[f] }

  type |||@[+F[f[_, _]]] = Lub[F, AC]
  type <+>@[+F[f[_, _]]] = Lub[F, AP]
  type |&|@[+F[f[_, _]]] = Lub[F, ACP]

  implicit val ar: Lub.Aux[AR, AR, AR] = new Lub[AR, AR] { type Lub[f[_, _]] = AR[f] }
}
trait LubArrow0 extends LubArrow1 {
  implicit val ac: Lub.Aux[AC, AC, AC] = new Lub[AC, AC] { type Lub[f[_, _]] = AC[f] }
}
trait LubArrow1 extends LubArrow2 {
  implicit val ap: Lub.Aux[AP, AP, AP] = new Lub[AP, AP] { type Lub[f[_, _]] = AP[f] }
}
trait LubArrow2 extends LubArrow3 {
  implicit val az: Lub.Aux[AZ, AZ, AZ] = new Lub[AZ, AZ] { type Lub[f[_, _]] = AZ[f] }
}
trait LubArrow3 extends LubArrow4 {
  implicit val acp: Lub.Aux[ACP, ACP, ACP] = new Lub[ACP, ACP] { type Lub[f[_, _]] = ACP[f] }
}
trait LubArrow4 {
  implicit val acz: Lub.Aux[ACZ, ACZ, ACZ] = new Lub[ACZ, ACZ] { type Lub[f[_, _]] = ACZ[f] }
}