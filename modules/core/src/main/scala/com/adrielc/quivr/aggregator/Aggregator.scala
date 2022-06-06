package com.adrielc.quivr.aggregator

import shapeless.{::, HList, HNil, Lazy, Witness}
import shapeless.labelled.{FieldType, field}

import scala.language.higherKinds
import cats.data.{NonEmptyList, ValidatedNel}
import cats.{Applicative, Apply, Contravariant, Foldable, Monoid, NonEmptyParallel, Semigroup, Semigroupal}
import cats.arrow.Strong
import cats.implicits._
import com.adrielc.quivr.aggregator.derived.DeriveAggregator

trait Aggregator[-In, +Out] extends Serializable {

  /**
   * The data type that is combined to form the aggregate
   */
  type Agg

  /**
   * Prepare/transform the input into its intermediate aggregation state
   * @param input
   * @return
   */
  def prepare(input: In): Agg

  /**
   * Provide a semigroup instance for the aggregate buffer [[Agg]] for aggregating/combining values
   *
   * @return
   */
  implicit def semigroup: Semigroup[Agg]

  /**
   * Transform the final aggregated state into its output form
   * @param reduction
   * @return
   */
  def present(reduction: Agg): Out


  def combine(a: Agg, b: Agg): Agg = semigroup.combine(a, b)

  def append[I <: In](a: Agg, i: I): Agg = combine(a, prepare(i))


  /**
   *
   * Functions for using this Aggregator to aggregate over a collection of [[In]]
   *
   * Each has parallel variant
   *
   */
  def reduce(i: In, is: In*): Out = present(is.aggregate(prepare(i))((agg, i) => append(agg, i), _ |+| _))

  def reduce(f: NonEmptyList[In]): Out = present(f.reduceLeftTo(prepare)(append))

  def reduceOption(f: TraversableOnce[In]): Option[Out] = parReduceOption(f)

  def reduceLeftToOption[F[_]: Foldable, I <: In](f: F[I]): Option[Out] = f.reduceLeftToOption(prepare)(append).map(present)

  def parReduce(f: NonEmptyList[In]): Out = present(f.reduceLeftTo(prepare)(append))

  def parReduceOption(f: TraversableOnce[In]): Option[Out] = aggTraversable(f)

  private def aggTraversable(s: TraversableOnce[In]): Option[Out] = s.aggregate(none[Agg])({
    case (Some(a), i) => append(a, i).some
    case (None, i) => prepare(i).some
  }, _ |+| _).map(present)

  /**
   *
   * Combinators for creating new Aggregators from existing ones
   *
   */

  /**
   * Transform the output to this Aggregator
   * @param f function to apply to the output of this Aggregator's `present` function
   */
  def map[C](f: Out => C): Aggregator.Aux[In, C, Agg] =
    Aggregator.instance(prepare, combine, present _ andThen f)


  /**
   * Transform the input to this Aggregator
   * @param f function to apply to the input prior to this Aggregator's `prepare` function
   */
  def contramap[C](f: C => In): Aggregator.Aux[C, Out, Agg] =
    Aggregator.instance(f andThen prepare, combine, present)

  /**
   * Only aggregate inputs that satisfy the given predicate
   */
  def filter[I <: In](f: I => Boolean): MonoidAggregator.Aux[I, Option[Out], Option[Agg]] =
    liftOption.contramap(Option(_).filter(f))

  def :>>[C](f: Out => C): Aggregator.Aux[In, C, Agg] = map(f)
  def >>:[C](f: C => In): Aggregator.Aux[C, Out, Agg] = contramap(f)


  /**
   * transform both the input and output of this type
   */
  def dimap[I, O](fX: I => In, fY: Out => O): Aggregator.Aux[I, O, Agg] =
    contramap(fX).map(fY)


  /**
   * Combine this aggregator with another, tupling their respective inputs and outputs
   */
  def zip[I, O](other: Aggregator[I, O]): Aggregator.Aux[(In, I), (Out, O), (Agg, other.Agg)] = {
    implicit val s = other.semigroup
    Aggregator.instanceSemigroup(a => (prepare(a._1), other.prepare(a._2)), present _ *** other.present)
  }

  def ***[I, O](other: Aggregator[I, O]): Aggregator.Aux[(In, I), (Out, O), (Agg, other.Agg)] = zip(other)

  /**
   * Combine this aggregator with another of the same input type, tupling the output types
   */
  def join[I <: In, O](other: Aggregator[I, O]): Aggregator.Aux[I, (Out, O), (Agg, other.Agg)] =
    zip(other).contramap[I](i => (i, i))

  def &&&[I <: In, O](other: Aggregator[I, O]): Aggregator.Aux[I, (Out, O), (Agg, other.Agg)] = join(other)

  /**
   * Similar to `join`, but combines the outputs to an HList instead of a tuple
   */
  def joinRecord[I <: In, O](other: Aggregator[I, O])(implicit J: JoinWith[Out, O]): Aggregator.Aux[I, J.Out, (Agg, other.Agg)] =
    join(other).map(o => J(o._1, o._2))

  /**
   * allows a value not wrapped in an [[M]] to be an input to this aggregator by lifting it into the [[M]] before calling [[present]]
   */
  def pureIn[I, M[_]](implicit A: Applicative[M], ev: M[I] <:< In): Aggregator.Aux[I, Out, Agg] =
    contramap(_.pure[M])

  /**
   * Given an empty [[Agg]] representation, converts this Aggregator to a MonoidAggregator
   */
  def withEmpty(empty: Agg): MonoidAggregator.Aux[In, Out, Agg] =
    MonoidAggregator.instance(prepare, combine, present, empty)

  /**
   * Given an [[Monoid]] instance for [[Agg]], converts this Aggregator to a MonoidAggregator
   */
  def liftMonoid(implicit M: Lazy[Monoid[Agg]]): MonoidAggregator.Aux[In, Out, Agg] =
    withEmpty(M.value.empty)

  /**
   * Lifts this Aggregator to a MonoidAggregator of the same type using the Option Monoid
   */
  def liftOption: MonoidAggregator.Aux[Option[In], Option[Out], Option[Agg]] =
    MonoidAggregator.instanceMonoid(_.map(prepare), _.map(present))

  /**
   * Aggregator => MonoidAggregator
   */
  def liftOptionPure: MonoidAggregator.Aux[In, Option[Out], Option[Agg]] =
    liftOption.pureIn[In, Option]

  /**
   * Lifts this Aggregator into an `F[_]` with an [[Apply]] instance available
   */
  def liftApply[F[+_] : Apply]: Aggregator.Aux[F[In], F[Out], F[Agg]] =
    Aggregator.instance[F[In], F[Out], F[Agg]](_.map(prepare), _.map2(_)(combine), _.map(present))

  /**
   * Lifts this Aggregator into an `F[_]` with an [[NonEmptyParallel]] instance available
   */
  def liftPar[F[+_]](implicit P: NonEmptyParallel[F]): Aggregator.Aux[F[In], F[Out], P.F[Agg]] = {
    val A = P.apply
    Aggregator.instance[F[In], F[Out], P.F[Agg]](
      i => A.map(P.parallel(i))(prepare),
      (a, b) => A.map2(a, b)(combine),
      o => P.sequential(A.map(o)(present))
    )
  }

  /**
   * Applies this Aggregator to the values in a [[Map]] with a key of type [[K]]
   */
  def liftMap[K]: MonoidAggregator.Aux[Map[K, In], Map[K, Out], Map[K, Agg]] =
    MonoidAggregator.instanceMonoid(_.mapValues(prepare), _.mapValues(present))

  /**
   * aggregate [[In]] if all are valid, or return accumulated errors [[E]] for all inputs that fail the predicate
   */
  def validated[I <: In, E](onFailure: I => E)(f: I => Boolean): ValidatedAgg[I, Out, E] =
    liftApply[ValidatedNel[E, +*]].contramap(i => if(f(i)) i.validNel else onFailure(i).invalidNel)

  /**
   * Have this Aggregator reduce a [[TraversableOnce]] collection of [[In]] prior to aggregating with `agg`
   * @param other Aggregator used to aggregate
   */
  def composeReduce[O](other: Aggregator[Out, O]): MonoidAggregator.Aux[TraversableOnce[In], Option[O], Option[other.Agg]] =
    other.liftOption.contramap(reduceOption)


  /**
   * Reduce the [[Out]] of this aggregator with another
   */
  def andThenReduce[O, B](other: Aggregator[O, B])(implicit ev: Out <:< TraversableOnce[O]): Aggregator.Aux[In, Option[B], Agg] =
    map(o => other.reduceOption(ev(o)))

  /**
   * Aggregate the [[Out]] of this aggregator with another
   */
  def andThenFold[O, B](other: MonoidAggregator[O, B])(implicit ev: Out <:< TraversableOnce[O]): Aggregator.Aux[In, B, Agg] =
    map(o => other.fold(ev(o)))

  /**
   *
   * Use this to tag the output with a phantom type that can be used to select the value out of an HList
   *
   * use `import aggregator.implicits._` for the field selection syntax (e.g. `result('mean)`)
   *
   * Example:
   * {{{
   *
   * scala> val recordAgg = Mean[Int].tag('mean) joinRecord Mode[Int].tag('mode)
   *
   * scala> val result = recordAgg.reduce(1, 1, 1, 2)
   *
   * scala> result('mean)
   * res1: Double = 1.25
   *
   * scala> result('mode)
   * res1: Int = 1
   *
   * }}}
   *
   * @param k A [[Symbol]] (or other valid [[Witness]] type) to tag the [[Out]] value with
   */
  def tag(k: Witness): Aggregator.Aux[In, FieldType[k.T, Out], Agg] = map(field[k.T](_))

  /**
   * Useful when implicitly deriving an Aggregator for a case class. An aggregator tagged with field name will override the aggregation behavior for
   * a particular field of the same type and field name
   *
   * Example:
   * {{{
   *
   * scala> implicit val longAgg: Aggregator[Long, Long] = Aggregator.Sum[Long]
   * scala> implicit val idAgg: FieldAggregator[Long, Long, Witness.`'id`.T] = Aggregator.Const(0L).forField('id)
   *
   * scala> case class Clicks(id: Long, clicks: Long)
   *
   * scala> val clicksAgg: Aggregator[Clicks, Clicks] = implicitly[Aggregator[Clicks, Clicks]] // ids are not summed
   * }}}
   *
   */
  def forField(k: Witness.Lt[Symbol]): FieldAggregator[In, Out, k.T] = FieldAggregator(tag(k))

  /**
   * prepends the inputs and outputs of this aggregator to the in/out HLists of the other
   */
  private [aggregator] def joinHList[TI <: HList, TO <: HList](other: Aggregator[TI, TO]): Aggregator.Aux[In :: TI, Out :: TO, (Agg, other.Agg)] = {
    implicit val s = other.semigroup
    Aggregator.instanceSemigroup(a => (prepare(a.head), other.prepare(a.tail)), b => present(b._1) :: other.present(b._2))
  }

  private[aggregator] def splitHListTuple[TI <: HList, TO <: HList](other: Aggregator[TI, TO]): Aggregator.Aux[In :: TI, Out :: TO, (Agg, other.Agg)] =
    Aggregator.instance(
      a => (prepare(a.head), other.prepare(a.tail)),
      (a, b) => (combine(a._1, b._1), other.combine(a._2, b._2)),
      b => present(b._1) :: other.present(b._2)
    )

  private[aggregator] def splitHListPrepend[TI <: HList, TO <: HList]
  (other: Aggregator[TI, TO])
  (implicit P: UndoPrepend[Agg, other.Agg]): Aggregator.Aux[In :: TI, Out :: TO, P.Out] =
    Aggregator.instance(
      a => P(prepare(a.head), other.prepare(a.tail)),
      (a, b) => {
        val (h1, h2) = P.undo(a)
        val (t1, t2) = P.undo(b)
        P(combine(h1, t1), other.combine(h2, t2))
      },
      b => {
        val (h, t) = P.undo(b)
        present(h) :: other.present(t)
      }
    )

  private[aggregator] def joinHListPrepend[I <: In, O <: HList]
  (other: Aggregator[I, O])
  (implicit P: UndoPrepend[Agg, other.Agg]): Aggregator.Aux[I, Out :: O, P.Out] =
    Aggregator.instance(
      a => P(prepare(a), other.prepare(a)),
      (a, b) => {
        val (h1, h2) = P.undo(a)
        val (t1, t2) = P.undo(b)
        P(combine(h1, t1), other.combine(h2, t2))
      },
      b => {
        val (h, t) = P.undo(b)
        present(h) :: other.present(t)
      }
    )

  private[aggregator] def joinHListTuple[I <: In, O <: HList, A](other: Aggregator.Aux[I, O, A]): Aggregator.Aux[I, Out :: O, (Agg, A)] =
    Aggregator.instance(
      a => (prepare(a), other.prepare(a)),
      (a, b) => (combine(a._1, b._1), other.combine(a._2, b._2)),
      b => present(b._1) :: other.present(b._2)
    )

  def tapInput[I <: In](tap: I => Unit): Aggregator.Aux[I, Out, Agg] = contramap { i => tap(i); i }

  def tapOutput(tap: Out => Unit): Aggregator.Aux[In, Out, Agg] = map { o => tap(o); o }
}

object Aggregator extends DeriveAggregator {

  type Aux[-I, +O, A] = Aggregator[I, O] { type Agg = A }

  @inline def apply[A, B](implicit A: Lazy[Aggregator[A, B]]): Aggregator.Aux[A, B, A.value.Agg] = A.value

  @inline def apply[A](implicit A: Lazy[Aggregator[A, A]], dummyImplicit: DummyImplicit): Aggregator.Aux[A, A, A.value.Agg] = A.value

  @inline def from[A]: AggFrom.Aux[A, HNil, HNil] = AggFrom[A]

  @inline def instance[I, O, A](prep: I => A, comb: (A, A) => A, pres: A => O): Aggregator.Aux[I, O, A] = instanceSemigroup(prep, pres)(Semigroup.instance(comb))

  @inline def instance[I](comb: (I, I) => I): Aggregator.Aux[I, I, I] = instanceSemigroup(Semigroup.instance(comb))

  @inline def instanceSemigroup[I, O, A](prep: I => A, pres: A => O)(implicit S: Semigroup[A]): Aggregator.Aux[I, O, A] = new Aggregator[I, O] {
    type Agg = A
    def prepare(input: I): Agg = prep(input)
    val semigroup: Semigroup[Agg] = S
    def present(reduction: Agg): O = pres(reduction)
  }

  @inline def instanceSemigroup[I : Semigroup]: Aggregator.Aux[I, I, I] = instanceSemigroup(identity, identity)

  // cats instances for Aggregator: for either left or right type param to take advantage of the arity functions in Cats

  implicit def aggSemigroupal[O](implicit S: Semigroup[O]): Semigroupal[Aggregator[*, O]] = new Semigroupal[Aggregator[*, O]] {

    def product[A, B](fa: Aggregator[A, O], fb: Aggregator[B, O]): Aggregator[(A, B), O] = (fa zip fb).map((S.combine _).tupled)
  }

  implicit def aggContravariant[O]: Contravariant[Aggregator[*, O]] = new Contravariant[Aggregator[*, O]] {

    def contramap[A, B](fa: Aggregator[A, O])(f: B => A): Aggregator[B, O] = fa.contramap(f)
  }

  implicit def aggApplicative[I]: Applicative[Aggregator[I, *]] = new Applicative[Aggregator[I, *]] {

    def pure[A](x: A): Aggregator[I, A] = Const(x)

    def ap[A, B](ff: Aggregator[I, A => B])(fa: Aggregator[I, A]): Aggregator[I, B] = (fa join ff).map { case (a, f) => f(a) }
  }

  implicit val aggStrong: Strong[Aggregator] = new Strong[Aggregator] {

    def first[A, B, C](fa: Aggregator[A, B]): Aggregator[(A, C), (B, C)] = fa zip First[C]

    def second[A, B, C](fa: Aggregator[A, B]): Aggregator[(C, A), (C, B)] = First[C] zip fa

    def dimap[A, B, C, D](fab: Aggregator[A, B])(f: C => A)(g: B => D): Aggregator[C, D] = fab.dimap(f, g)
  }

  implicit def toAggApplicativeBuilder[I, O, A](agg: Aggregator.Aux[I, O, A]): AggBuilder[I, Aggregator.Aux[I, O, A] :: HNil, O :: HNil] = AggBuilder(agg)
}