package com.adrielc.quivr.aggregator

import cats.{Foldable, Monoid}
import cats.data.Chain
import cats.kernel.Semigroup
import shapeless.{::, Generic, HNil, Lazy}


import scala.collection.mutable

trait KeyValue[A] extends Serializable {

  type Key

  type Value

  def key(a: A): Key

  def value(a: A): Value

  def fromKeyValue(k: Key, v: Value): A

  def fromKeyValue(tuple: (Key, Value)): A = fromKeyValue(tuple._1, tuple._2)

  def toKeyValue(a: A): (Key, Value) = key(a) -> value(a)

  def toMap(a: A): Map[Key, Value] = Map(toKeyValue(a))

  def fromMap(m: Iterable[(Key, Value)]): List[A] = m.map(fromKeyValue).toList

  import cats.implicits._

  def lastByKey[F[_] : Foldable](f: F[A]): Map[Key, Value] = f.foldMapK[Map[Key, *], Value](toMap)
}

object KeyValue {

  import cats.implicits._

  type Aux[A, K, V] = KeyValue[A] { type Key = K; type Value = V }

  @inline def apply[A, K, V](implicit K: KeyValue.Aux[A, K, V]): KeyValue.Aux[A, K, V] = K

  @inline def apply[A](implicit K: Lazy[KeyValue[A]], dummyImplicit: DummyImplicit): KeyValue.Aux[A, K.value.Key, K.value.Value] = K.value

  @inline def instance[A, K, V](toKey: A => K, toValue: A => V, fromKV: (K, V) => A): KeyValue.Aux[A, K, V] = new KeyValue[A] {
    type Key = K; type Value = V
    def key(a: A): Key = toKey(a)
    def value(a: A): Value = toValue(a)
    def fromKeyValue(k: Key, v: Value): A = fromKV(k, v)
  }

  implicit class KeyValueOps[M[_], A](private val m: M[A]) extends AnyVal {

    def lastByKey(implicit F: Foldable[M], K: KeyValue[A]): Map[K.Key, K.Value] = K.lastByKey(m)

    def combineByKey(implicit F: Foldable[M], K: KeyCombineValue[A]): Map[K.Key, K.Value] = K.combineByKey(m)

    def averageByKey(implicit F: Foldable[M], K: KeyAverageValue[A]): Map[K.Key, K.Value] = K.averageByKey(m)
  }

  implicit def kvGeneric[A, K, V](implicit G: Lazy[Generic.Aux[A, K :: V :: HNil]]): KeyValue.Aux[A, K, V] =
    instance(G.value.to(_).head, G.value.to(_).tail.head, (k, v) => G.value.from(k :: v :: HNil))


  trait KeyCombineValue[A] extends KeyValue[A] {

    implicit def valueSemigroup: Semigroup[Value]

    final lazy val agg: MonoidAggregator.Aux[A, List[A], Map[Key, Value]] = Fold[Map[Key, Value]].dimap(toMap, fromMap)

    def combineByKey[F[_] : Foldable](f: F[A]): Map[Key, Value] = f.foldMap(toMap)
  }

  object KeyCombineValue {

    type Aux[A, K, V] = KeyCombineValue[A] { type Key = K; type Value = V }

    def apply[A](implicit K: KeyCombineValue[A]): KeyCombineValue.Aux[A, K.Key, K.Value] = K

    implicit def fromKeyValue[A, K, V](implicit K: Lazy[KeyValue.Aux[A, K, V]], S: Lazy[Semigroup[V]]): KeyCombineValue.Aux[A, K, V] = new KeyCombineValue[A] {
      type Key = K; type Value = V
      def key(a: A): K = K.value.key(a)
      def value(a: A): V = K.value.value(a)
      def fromKeyValue(k: K, v: V): A = K.value.fromKeyValue(k, v)
      def valueSemigroup: Semigroup[V] = S.value
    }
  }

  trait KeyAverageValue[A] extends KeyCombineValue[A] {

    type Value = Double

    def averageByKey[F[_] : Foldable](f: F[A]): Map[Key, Double] = f.foldMap(toAverageMap).mapValues(_.average)

    override val valueSemigroup: Semigroup[Double] = Semigroup[Double]

    def toAverageMap(a: A): AverageMap[Key] = Map(key(a) -> AveragedValue(value(a)))

    def fromAverageMap(m: Iterable[(Key, AveragedValue)]): List[A] = m.map { case (k, v) => fromKeyValue(k, v.average) }.toList

    final val aggAvg: MonoidAggregator.Aux[A, AverageMap[Key], Chain[(Key, Value)]] =
      Fold[Chain[(Key, Value)]].dimap(
        (a: A) => Chain.apply((key(a), value(a))),
        computeAverageMap
      )

    final val aggFromAvg: MonoidAggregator.Aux[A, List[A], Chain[(Key, Value)]] = aggAvg.map(fromAverageMap)

    final def aggTopKFromNestedAvg(k: Int): MonoidAggregator.Aux[TraversableOnce[A], List[A], Chain[(Key, Value)]] =
      Fold[Chain[(Key, Value)]]
        .nested
        .dimap(
          (t: TraversableOnce[A]) => t.map(a => Chain.apply((key(a), value(a)))),
          (c: Chain[(Key, Value)]) => fromAverageMap(computeAverageMap(c).topK(k))
        )

    private def computeAverageMap(chain: Chain[(Key, Value)]): AverageMap[Key] = {
      val acc = mutable.Map.empty[Key, AveragedValue]
      val m = Monoid[AveragedValue]
      chain.iterator.foreach { case (k, v) =>
        val curr = acc.getOrElse(k, m.empty)
        acc.update(k, curr.combine(AveragedValue(v)))
      }
      acc.toMap
    }
  }

  object KeyAverageValue {

    type Aux[A, K] = KeyAverageValue[A] { type Key = K }

    def apply[A, K](implicit K: KeyAverageValue.Aux[A, K]): KeyAverageValue.Aux[A, K] = K

    def apply[A](implicit K: Lazy[KeyAverageValue[A]], dummyImplicit: DummyImplicit): KeyAverageValue.Aux[A, K.value.Key] = K.value

    implicit def fromKeyValue[A, K](implicit K: KeyValue.Aux[A, K, Double]): KeyAverageValue.Aux[A, K] = new KeyAverageValue[A] {
      type Key = K
      def key(a: A): K = K.key(a)
      def value(a: A): Double = K.value(a)
      def fromKeyValue(k: K, v: Double): A = K.fromKeyValue(k, v)
    }
  }
}