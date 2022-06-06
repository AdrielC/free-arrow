package com.adrielc.quivr

import cats.{Alternative, Applicative, Monoid, MonoidK, Order, Semigroup, SemigroupK}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, ValidatedNel}
import com.adrielc.quivr.aggregator.MonoidAggregator.instanceMonoid
import com.adrielc.quivr.aggregator.Aggregator.instanceSemigroup
import shapeless.syntax.RecordOps
import shapeless.syntax.std.tuple._
import shapeless.ops.hlist.Tupler
import shapeless.ops.function.FnToProduct
import shapeless.ops.tuple.FlatMapper
import shapeless.ops.product.ToTuple
import cats.Applicative.ToApplicativeOps
import cats.implicits._
import com.adrielc.quivr.aggregator.KeyValue._
import shapeless.{Generic, HList, HNil, Lazy, Poly1}
import shapeless.labelled.FieldType
import shapeless.ops.record.{AlignByKeys, Keys}
import shapeless._
import ops.hlist.Prepend

import scala.collection.{TraversableOnce, mutable}

package object aggregator {

  type UnaryAgg[A] = Aggregator[A, A]

  type UnaryMonoidAgg[A] = MonoidAggregator[A, A]

  type ValidatedAgg[I, +O, E] = Aggregator[I, ValidatedNel[E, O]]

  type ValidatedMonoidAgg[I, O, E] = MonoidAggregator[I, ValidatedNel[E, O]]

  type IdFieldAgg[I] = FieldAggregator[I, I, Witness.`'id`.T]

  type TraverseAgg[A, B] = MonoidAggregator[A, TraversableOnce[B]]

  type AverageMap[K] = Map[K, AveragedValue]


  type CountMap[K] = Map[K, Long]
  implicit val countMapAlt: Alternative[CountMap] = new Alternative[CountMap] {
    def pure[A](x: A): CountMap[A] = CountMap(x)
    def empty[A]: CountMap[A] = CountMap.empty
    def ap[A, B](ff: CountMap[A => B])(fa: CountMap[A]): CountMap[B] =
      for {
        (a, c1) <- fa
        (f, c2) <- ff
      } yield f(a) -> (c1 + c2)

    override def map[A, B](fa: CountMap[A])(f: A => B): CountMap[B] =
      fa.map { case (k, c) => f(k) -> c }

    def combineK[A](x: CountMap[A], y: CountMap[A]): CountMap[A] =
      x |+| y
  }

  implicit def recordOps[L <: HList](l : L): RecordOps[L] = new RecordOps(l)

  implicit class OrderMapOps[K, V](private val m: Iterable[(K, V)]) extends AnyVal {
    def topK(k: Int)(implicit O: Order[V]): List[(K, V)] = m.toList.sortBy(_._2)(O.toOrdering.reverse).take(k)
    def topKKeys(k: Int)(implicit O: Order[V]): List[K] = topK(k).map(_._1)
  }

  implicit class CaseClassToTuple[C <: Product](private val c: C) extends AnyVal {
    def toTuple[O](implicit t: ToTuple.Aux[C, O]): O = t(c)
  }

  implicit class HListToTuple[C <: HList](private val c: C) extends AnyVal {
    def toTuple[O](implicit t: shapeless.ops.hlist.Tupler.Aux[C, O]): O = t(c)
  }


  /**
   * Constructors for common aggregations
   */
  object Reduce {

    @inline def apply[A](combine: (A, A) => A): Aggregator.Aux[A, A, A] = Aggregator.instance(combine)

    @inline def apply[A : Semigroup]: Aggregator.Aux[A, A, A] = instanceSemigroup

    @inline def K[A , M[_] : SemigroupK]: Aggregator.Aux[M[A], M[A], M[A]] = instanceSemigroup(SemigroupK[M].algebra)

    object either {

      @inline def par[I, O: Semigroup, E](f: I => Either[E, O]): Aggregator[I, Either[NonEmptyList[E], O]] =
        Reduce(Semigroup[O]).liftPar[Either[NonEmptyList[E], +*]].contramap(f(_).leftMap(NonEmptyList.one))

      @inline def apply[I, O: Semigroup, E](f: I => Either[E, O]): Aggregator[I, Either[E, O]] =
        Reduce(Semigroup[O]).liftApply[Either[E, +*]].contramap(f)
    }
  }

  object Fold {

    @inline def apply[A](combine: (A, A) => A, empty: A): MonoidAggregator.Aux[A, A, A] = MonoidAggregator.instance(empty, combine)

    @inline def apply[A : Monoid]: MonoidAggregator.Aux[A, A, A] = instanceMonoid

    @inline def K[A , M[_] : MonoidK]: MonoidAggregator.Aux[M[A], M[A], M[A]] = instanceMonoid(MonoidK[M].algebra)
  }

  object First {

    @inline def apply[T]: Aggregator.Aux[T, T, T] = Aggregator.instance((a, _) => a)
  }

  object Last {

    @inline def apply[T]: Aggregator.Aux[T, T, T] = Aggregator.instance((_, a) => a)
  }

  object Const {

    @inline def apply[O](out: O): MonoidAggregator.Aux[Any, O, String] = MonoidAggregator.instance(_ => "", (a, _) => a, _ => out, "")
  }

  object Collect {

    @inline def alternative[I, M[_] : Alternative]: MonoidAggregator.Aux[I, M[I], M[I]] =
      Fold.K[I, M].pureIn[I, M]

    @inline def semigroupK[I, M[_] : Applicative : SemigroupK]: Aggregator.Aux[I, M[I], M[I]] =
      Reduce.K[I, M].pureIn[I, M]

    @inline def set[I]: MonoidAggregator.Aux[I, Set[I], Set[I]] = instanceMonoid[I, Set[I], Set[I]](Set(_), identity)(MonoidK[Set].algebra)

    @inline def vector[I]: MonoidAggregator.Aux[I, Vector[I], Vector[I]] = alternative[I, Vector]

    @inline def list[I, A](f: I => A): MonoidAggregator.Aux[I, List[A], Chain[A]] = f >>: list[A]

    @inline def list[I]: MonoidAggregator.Aux[I, List[I], Chain[I]] = alternative[I, Chain] :>> (_.toList)

    @inline def chain[I, A](f: I => A): MonoidAggregator.Aux[I, Chain[A], Chain[A]] = f >>: alternative[A, Chain]

    @inline def nec[I, A](f: I => A): Aggregator.Aux[I, NonEmptyChain[A], NonEmptyChain[A]] = f >>: semigroupK[A, NonEmptyChain]

    @inline def nel[I, A](f: I => A): Aggregator.Aux[I, NonEmptyList[A], NonEmptyChain[A]] = nec(f) :>> (_.toNonEmptyList)

    object either {

      @inline def par[I, O, E](f: I => Either[E, O]): Aggregator[I, Either[NonEmptyList[E], NonEmptyList[O]]] =
        Reduce.either.par(f(_).map(NonEmptyList.one))

      @inline def apply[I, O, E](f: I => Either[E, O]): Aggregator[I, Either[E, NonEmptyList[O]]] =
        Reduce.either(f(_).map(NonEmptyList.one))
    }
  }

  object Count {

    val each: MonoidAggregator.Aux[Any, Long, Long] = Fold[Long].contramap(_ => 1L)

    @inline def ifCondition[I](countIf: I => Boolean): MonoidAggregator.Aux[I, Long, Long] = Fold[Long].contramap(i => if(countIf(i)) 1 else 0)

    object distinct {

      @inline def apply[I]: MonoidAggregator.Aux[I, Long, Set[I]] = Collect.set[I] :>> (_.size.toLong)

      def toCountMap[I](chain: Chain[I]): CountMap[I] = {
        val acc = mutable.Map.empty[I, Long]
        chain.iterator.foreach { i =>
          acc.update(i, acc.getOrElse(i, 0L) + 1L)
        }
        acc.toMap
      }

      @inline def map[I]: MonoidAggregator.Aux[I, CountMap[I], Chain[I]] =
        Fold[Chain[I]].dimap(
          (i: I) => Chain.apply(i),
          toCountMap
        )
    }
  }

  object Min {

    def apply[A](implicit O: Order[A]): Aggregator.Aux[A, A, A] = Aggregator.instance(O.min)
  }

  object Max {

    def apply[A](implicit O: Order[A]): Aggregator.Aux[A, A, A] = Aggregator.instance(O.max)
  }

  object Prop {

    @inline def apply[I](countIf: I => Boolean): Aggregator.Aux[I, Double, AveragedValue] = Mean(i => if(countIf(i)) 1 else 0)
  }

  object Mean {
    @inline def apply[I](toValue: I => Double): MonoidAggregator.Aux[I, Double, AveragedValue] = AveragedValue.averageOf(toValue)

    @inline def apply[N: Numeric]: Aggregator.Aux[N, Double, AveragedValue] = AveragedValue.averageOf[N]

    object Round {

      @inline def apply[N : Numeric]: Aggregator.Aux[N, N, AveragedValue] = AveragedValue.roundedAverageOf[N]
    }

    object Map {

      @inline def apply[K, V : Numeric]: MonoidAggregator.Aux[Map[K, V], Map[K, Double], AverageMap[K]] = AveragedValue.averageOf[V].liftMap

      @inline def apply[I](implicit K: KeyAverageValue[I]): MonoidAggregator.Aux[I, AverageMap[K.Key], Chain[(K.Key, K.Value)]] = K.aggAvg
    }
  }

  object TopK {

    @inline def apply[I](k: Int, minimum: Int = 0): MonoidAggregator.Aux[I, List[I], Chain[I]] =
      Count.distinct.map[I].map(topAboveThreshold(k, minimum, _))

    object Nested {

      @inline def apply[I](k: Int, minimum: Int = 0): MonoidAggregator.Aux[TraversableOnce[I], List[I], Chain[I]] = {
        Fold[Chain[I]]
          .nested
          .dimap(
            (t: TraversableOnce[I]) => t.map(Chain(_)),
            c => topAboveThreshold(k, minimum, Count.distinct.toCountMap(c))
          )
      }
    }

    private def topAboveThreshold[I](k: Int, minimum: Int, counts: CountMap[I]) = {
      val highest = if (counts.nonEmpty) counts.maxBy(_._2)._2 else 0
      val threshold = math.min(minimum.toLong, highest)
      counts.filter(_._2 >= threshold).topKKeys(k)
    }

    object Map {

      @inline def apply[K, V](k: Int, minimum: Int = 0): MonoidAggregator.Aux[Map[K, V], Map[K, V], Chain[(K, V)]] =
        Fold[Chain[(K, V)]]
          .nested
          .contramap((m: Map[K, V]) => m.map(Chain(_)))
          .map((c: Chain[(K, V)]) => topAboveThreshold(k, minimum, Count.distinct.toCountMap(c)).toMap)
    }

    object ScoredSeq {
      @inline def apply[I](k: Int)(implicit K: KeyAverageValue[I]): MonoidAggregator.Aux[TraversableOnce[I], List[I], Chain[(K.Key, K.Value)]] = K.aggTopKFromNestedAvg(k)
    }
  }

  object Mode {

    @inline def apply[I]: Aggregator.Aux[I, I, Chain[I]] = TopK(1).map(_.head)
  }

  object Validated {

    @inline def reduction[A: Semigroup, E](onFailure: A => E)(f: A => Boolean): ValidatedAgg[A, A, E] = Reduce(Semigroup[A]).validated(onFailure)(f)

    @inline def fold[A: Monoid, E](onFailure: A => E)(f: A => Boolean): ValidatedAgg[A, A, E] = Fold(Monoid[A]).validated(onFailure)(f)
  }

  /**
   * Flattens nested tuples
   *
   * Example:
   * {{{
   *
   * scala> val agg: Aggregator[Int, ((Double, Int), Int)] = Mean[Int] join Mode[Int] join Fold[Int]
   *
   * scala> val aggFlat: Aggregator[Int, (Double, Int, Int)] = agg.map(flattenTuple)
   *
   * }}}
   */
  object flattenTuple extends LowPriorityFlatten {
    implicit def caseTuple[P <: Product](implicit lfm: Lazy[FlatMapper[P, flattenTuple.type]]) = at[P](lfm.value(_))
  }
}


package aggregator {

  case class FieldAggregator[-I, +O, K](ag: Aggregator[I, FieldType[K, O]])

  object CountMap {
    def apply[K](key: K): CountMap[K] = Map(key -> 1L)
    def empty[A]: CountMap[A] = Map.empty
  }


  trait LowPriorityFlatten extends Poly1 {
    implicit def default[T] = at[T](Tuple1(_))
  }

  /**
   * Type-class that computes a recursively-flattened [[HList]] `F` for an input type [[In]].
   *
   * Case-classes and [[HList]]s are expanded.
   */
  trait Flatten[In] extends Serializable {
    type Out <: HList
    def apply(l: In): Out
  }

  trait LowestPri {
    type Aux[In, Out0] = Flatten[In] { type Out = Out0 }

    def make[In, Out0 <: HList](fn: In ⇒ Out0): Aux[In, Out0] =
      new Flatten[In] {
        type Out = Out0
        override def apply(l: In) = fn(l)
      }
  }

  trait LowPriFlattenedImplicits extends LowestPri {
    // prepend an element directly if it can't be flattened further (via higher-priority implicits below)
    implicit def directCons[H, T <: HList, FT <: HList](implicit ft: Lazy[Flatten.Aux[T, FT]]):
    Aux[H ::  T, H :: FT] =
      make {
        case h :: t => h :: ft.value(t)
      }
  }

  object Flatten extends LowPriFlattenedImplicits {

    def apply[In](implicit flat: Flatten[In]): Flatten.Aux[In, flat.Out] = flat

    implicit val hnil: Flatten.Aux[HNil, HNil] = make(l => l)

    // Flatten and prepend a Product (e.g. case-class)
    implicit def nestedCCCons[H <: Product, FH <: HList, T <: HList, FT <: HList, Out <: HList]
    (implicit
     fh : Lazy[Flatten.Aux[H, FH]],
     ft : Lazy[Flatten.Aux[T, FT]],
     ++ : Prepend.Aux[FH, FT, Out]
    ): Flatten.Aux[H :: T, Out] =
      make { case h :: t =>
        ++(
          fh.value(h),
          ft.value(t)
        )
      }

    // Flatten a case-class directly
    implicit def cc[CC <: Product, L <: HList, FL <: HList]
    (implicit
     gen: Generic.Aux[CC, L],
     flat: Lazy[Aux[L, FL]]
    ): Aux[CC, FL] = make(cc => flat.value(gen.to(cc)))
  }

  trait JoinWith[-A, B] {
    type Out <: HList
    def apply(a: A, b: B): Out
  }
  object JoinWith extends JoinWith0 {
    implicit def withHLists[A <: HList, B <: HList](implicit p: Prepend[A, B]): Aux[A, B, p.Out] = instance(_ ++ _)
  }
  trait JoinWith0 extends JoinWith1 {
    implicit def withSingle[A <: HList, B](implicit p: Prepend[A, B :: HNil]): Aux[A, B, p.Out] = instance(_ :+ _)
  }
  trait JoinWith1 extends JoinWith2 {
    implicit def withInitHList[A, B <: HList]: Aux[A, B, A :: B] = instance(_ +: _)
  }
  trait JoinWith2 {
    implicit def withInit[A, B]: Aux[A, B, A :: B :: HNil] = instance(_ :: _ :: HNil)
    type Aux[A, B, O] = JoinWith[A, B] { type Out = O }
    def instance[A, B, O <: HList](f: (A, B) => O): Aux[A, B, O] = new JoinWith[A, B] { type Out = O; def apply(a: A, b: B): Out = f(a, b) }
  }

  trait IsHListOfAgg[In, I <: HList, Out <: HList] extends ToApplicativeOps with Serializable {
    type Agg
    def hsequence(l: I): Aggregator.Aux[In, Out, Agg]
  }

  object IsHListOfAgg extends IsHLisOfAgg0 {
    implicit def hconsPrependTuple[I, A, A1, H, Aggregators <: HList, Out <: HList](implicit
                                                                                    I: IsHListOfAgg.Aux[I, A1, Aggregators, Out],
                                                                                    P: UndoPrepend[A, A1]
                                                                                   ): IsHListOfAgg.Aux[I, P.Out, Aggregator.Aux[I, H, A] :: Aggregators, H :: Out] =
      new IsHListOfAgg[I, Aggregator.Aux[I, H, A] :: Aggregators, H :: Out] {
        type Agg = P.Out
        override def hsequence(l: Aggregator.Aux[I, H, A] :: Aggregators): Aggregator.Aux[I, H :: Out, P.Out] =
          l.head joinHListPrepend I.hsequence(l.tail)
      }
  }

  trait IsHLisOfAgg0 {

    implicit def hconsPrependTuple2[I, A, A1, H, Aggregators <: HList, Out <: HList](implicit
                                                                                     I: IsHListOfAgg.Aux[I, A1, Aggregators, Out]
                                                                                    ): IsHListOfAgg.Aux[I, (A, I.Agg), Aggregator.Aux[I, H, A] :: Aggregators, H :: Out] =
      new IsHListOfAgg[I, Aggregator.Aux[I, H, A] :: Aggregators, H :: Out] {
        type Agg = (A, I.Agg)
        override def hsequence(l: Aggregator.Aux[I, H, A] :: Aggregators): Aggregator.Aux[I, H :: Out, (A, A1)] =
          l.head joinHListTuple I.hsequence(l.tail)
      }

    implicit def isHListofAgg[I, A, O]: IsHListOfAgg.Aux[I, A, Aggregator.Aux[I, O, A] :: HNil, O :: HNil] =
      new IsHListOfAgg[I, Aggregator.Aux[I, O, A] :: HNil, O :: HNil] {
        type Agg = A
        def hsequence(l: Aggregator.Aux[I, O, A] :: HNil): Aggregator.Aux[I, O :: HNil, A] = l.head.map(HList(_))
      }


    type Aux[In, A, I <: HList, Out <: HList] = IsHListOfAgg[In, I, Out] { type Agg = A }
  }

  class AggBuilder[In, Aggregators <: HList, Out <: HList] private[aggregator] (val values: Aggregators) extends Serializable {

    def apply[F, FOut](f: F)(implicit F: FnToProduct.Aux[F, Out => FOut], I: IsHListOfAgg[In, Aggregators, Out]): Aggregator.Aux[In, FOut, I.Agg] =
      I.hsequence(values).map(F(f))

    def hlist(implicit I: IsHListOfAgg[In, Aggregators, Out]): Aggregator.Aux[In, Out, I.Agg] =
      I.hsequence(values)

    def as[CC <: Product](implicit
                          I: IsHListOfAgg[In, Aggregators, Out],
                          A: Aligner[Out, CC]
                         ): Aggregator.Aux[In, CC, I.Agg] =
      I.hsequence(values).map(A(_))

    def tupled[T](implicit I: IsHListOfAgg[In, Aggregators, Out], T: Tupler.Aux[Out, T]): Aggregator.Aux[In, T, I.Agg] =
      I.hsequence(values).map(_.tupled)

    def :&:[X, A1](other: Aggregator.Aux[In, X, A1]): AggBuilder[In, Aggregator.Aux[In, X, A1] :: Aggregators, X :: Out] =
      new AggBuilder[In, Aggregator.Aux[In, X, A1] :: Aggregators, X :: Out](other :: values)
  }


  trait Aligner[A <: HList, B] extends Serializable {
    def apply(a: A): B
  }
  object Aligner {
    implicit def align[A <: HList, B <: Product, Repr <: HList, K <: HList]
    (implicit
     G: LabelledGeneric.Aux[B, Repr],
     K: Keys.Aux[Repr, K],
     A: AlignByKeys.Aux[A, K, Repr]
    ): Aligner[A, B] = new Aligner[A, B] {
      val _ = K
      def apply(a: A): B = G.from(A(a))
    }
  }

  object AggBuilder {
    def apply[I, O, A](agg: Aggregator.Aux[I, O, A]): AggBuilder[I, Aggregator.Aux[I, O, A] :: HNil, O :: HNil] = new AggBuilder(HList(agg))
  }

  trait UndoPrepend[T, U] extends Serializable {
    type Out
    def apply(t: T, u: U): Out
    def undo(out: Out): (T, U)
  }
  object UndoPrepend {
    def apply[T, U](implicit prepend: UndoPrepend[T, U]): Aux[T, U, prepend.Out] = prepend

    type Aux[T, U, Out0] = UndoPrepend[T, U] { type Out = Out0 }

    implicit def prepend[T, U]: Aux[T, U, (T, U)] =
      new UndoPrepend[T, U] {
        type Out = (T, U)
        def apply(t: T, u: U): Out = (t, u)
        def undo(out: Out): (T, U) = out
      }
  }
}
