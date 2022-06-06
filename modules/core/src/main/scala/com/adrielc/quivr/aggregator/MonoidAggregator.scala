package com.adrielc.quivr.aggregator

import cats.{Applicative, Monoid}
import cats.kernel.Semigroup
import shapeless.Witness
import shapeless.labelled.{FieldType, field}
import cats.implicits._
import MonoidAggregator._

import scala.collection.GenSeq

trait MonoidAggregator[-In, +Out] extends Aggregator[In, Out] {

  implicit def monoid: Monoid[Agg]

  override def semigroup: Semigroup[Agg] = monoid

  def empty: Agg = monoid.empty

  def emptyOut: Out = present(empty)

  private def foldSeq(s: GenSeq[In]): Out = present(s.foldLeft(empty)(append))

  def fold(f: In*): Out = foldSeq(f.toSeq)

  def fold(f: TraversableOnce[In]): Out = foldSeq(f.toSeq)

  def parFold(f: TraversableOnce[In]): Out = foldSeq(f.toSeq.par)

  def composeFold[O, A](other: Aggregator[Out, O]): Aggregator.Aux[TraversableOnce[In], O, other.Agg] =
    other.contramap(fold)

  def composeFold[O, A](other: MonoidAggregator[Out, O]): MonoidAggregator.Aux[TraversableOnce[In], O, other.Agg] =
    other.contramap(fold)

  def nested: MonoidAggregator.Aux[TraversableOnce[In], Out, Agg] = instanceMonoid(_.aggregate(empty)(append, combine), present)

  override def map[C](f: Out => C): MonoidAggregator.Aux[In, C, Agg] =
    instance(prepare, combine, present _ andThen f, empty)

  override def contramap[C](f: C => In): MonoidAggregator.Aux[C, Out, Agg] =
    instance(prepare _ compose f, combine, present, empty)


  def zip[I, O, A](other: MonoidAggregator[I, O]): MonoidAggregator.Aux[(In, I), (Out, O), (Agg, other.Agg)] = {
    implicit val s = other.monoid
    MonoidAggregator.instanceMonoid(a => (prepare(a._1), other.prepare(a._2)), present _ *** other.present)
  }

  def join[I <: In, O](other: MonoidAggregator[I, O]): MonoidAggregator.Aux[I, (Out, O), (Agg, other.Agg)] =
    zip(other).contramap[I](i => (i, i))

  override def :>>[C](f: Out => C): MonoidAggregator.Aux[In, C, Agg] = map(f)
  override def >>:[C](f: C => In): MonoidAggregator.Aux[C, Out, Agg] = contramap(f)

  override def dimap[I, O](fX: I => In, fY: Out => O): MonoidAggregator.Aux[I, O, Agg] = contramap(fX).map(fY)

  override def andThenReduce[O, B](other: Aggregator[O, B])
                                  (implicit ev: Out <:< TraversableOnce[O]): MonoidAggregator.Aux[In, Option[B], Agg] =
    map(other.reduceOption(_))

  override def andThenFold[O, B](other: MonoidAggregator[O, B])
                                (implicit ev: Out <:< TraversableOnce[O]): MonoidAggregator.Aux[In, B, Agg] =
    map(other.fold(_))

  override def pureIn[I, M[_]](implicit A: Applicative[M], ev: M[I] <:< In): MonoidAggregator.Aux[I, Out, Agg] = contramap[I](i => ev(i.pure[M]))

  override def tag(k: Witness): MonoidAggregator.Aux[In, FieldType[k.T, Out], Agg] = map(field[k.T](_))

  override def tapInput[I <: In](tap: I => Unit): MonoidAggregator.Aux[I, Out, Agg] = contramap { i => tap(i); i }
}

object MonoidAggregator {

  type Aux[-I, +O, A] = MonoidAggregator[I, O] { type Agg = A }

  @inline def apply[A, B](implicit aggregator: MonoidAggregator[A, B]): MonoidAggregator.Aux[A, B, aggregator.Agg] = aggregator

  @inline def apply[A](implicit aggregator: MonoidAggregator[A, A], d: DummyImplicit): MonoidAggregator.Aux[A, A, aggregator.Agg] = aggregator

  @inline def instanceMonoid[I, O, A](prep: I => A, pres: A => O)(implicit M: Monoid[A]): MonoidAggregator.Aux[I, O, A] =
    new MonoidAggregator[I, O] {
      type Agg = A
      def prepare(input: I): Agg = prep(input)
      def present(reduction: Agg): O = pres(reduction)
      val monoid: Monoid[Agg] = M
    }

  @inline def instanceMonoid[I: Monoid]: MonoidAggregator.Aux[I, I, I] = instanceMonoid(identity, identity)

  @inline def instance[In, Out, B](prep: In => B, comb: (B, B) => B, pres: B => Out, zero: B): MonoidAggregator.Aux[In, Out, B] = instanceMonoid(prep, pres)(Monoid.instance(zero, comb))

  @inline def instance[I](zero: I, comb: (I, I) => I): MonoidAggregator.Aux[I, I, I] = instanceMonoid(Monoid.instance(zero, comb))
}