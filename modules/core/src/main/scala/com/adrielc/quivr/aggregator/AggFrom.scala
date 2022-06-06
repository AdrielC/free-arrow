package com.adrielc.quivr.aggregator

import cats.Order
import shapeless.labelled.FieldType
import shapeless.ops.hlist
import shapeless.{::, HList, HNil, Lazy, Witness}

trait AggFrom[In, Out <: HList] extends Serializable { self =>

  type Aggregators <: HList

  def agg: AggBuilder[In, Aggregators, Out]

  def ++[O <: HList, F <: HList](other: AggFrom[In, O])
                    (implicit P: hlist.Prepend[O, Out], F: Flatten[other.Aggregators :: self.Aggregators]): AggFrom.Aux[In, P.Out, F.Out] =
    new AggFrom[In, P.Out] {
      type Aggregators = F.Out
      def agg: AggBuilder[In, Aggregators, P.Out] = new AggBuilder(F.apply(other.agg.values :: self.agg.values))
    }

  def as[CC <: Product](implicit I: Lazy[IsHListOfAgg[In, Aggregators, Out]], A: Lazy[Aligner[Out, CC]]): Aggregator.Aux[In, CC, I.value.Agg] =
    agg.as[CC](I.value, A.value)

  def count(k: Witness)= add(Count.each)(k)

  def countIf[K](f: In => Boolean)(k: Witness)= add(Count.ifCondition(f))(k)

  def countDistinct[K](f: In => K)(k: Witness)= add(f, Count.distinct[K])(k)

  def collect[A](f: In => A)(k: Witness)= add(Collect.list(f))(k)

  def mean(f: In => Double)(k: Witness)= add(Mean(f))(k)

  def min[A: Order](f: In => A)(k: Witness)= add(f, Min[A])(k)

  def max[A: Order](f: In => A)(k: Witness)= add(f, Max[A])(k)

  def topK[A](f: In => A, top: Int, minK: Int = 0)(k: Witness)= add(f, TopK[A](top, minK))(k)

  def topKSeq[A](f: In => Seq[A], top: Int, minK: Int = 0)(k: Witness)= add(f, TopK.Nested[A](top, minK))(k)

  def add[A, O](f: In => A, aggregator: Aggregator[A, O])(k: Witness)= add[O](aggregator.contramap(f))(k)

  def add[O](aggregator: Aggregator[In, O])(k: Witness) =
    new AggFrom[In, FieldType[k.T, O] :: Out] {
      type Aggregators = Aggregator.Aux[In, FieldType[k.T, O], aggregator.Agg] :: self.Aggregators
      def agg: AggBuilder[In, Aggregators, FieldType[k.T, O] :: Out] = self.agg.:&:(aggregator.tag(k))
    }
}

object AggFrom {
  type Aux[I, O <: HList, Aggs <: HList] = AggFrom[I, O] { type Aggregators = Aggs }

  def apply[In]: AggFrom.Aux[In, HNil, HNil] = new AggFrom[In, HNil] {
    type Aggregators = HNil
    def agg: AggBuilder[In, HNil, HNil] = new AggBuilder(HNil)
  }
}