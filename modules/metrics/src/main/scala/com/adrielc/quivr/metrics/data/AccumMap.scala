package com.adrielc.quivr.metrics.data

import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem}
import cats.{Monad, Monoid, Order, Semigroup, SemigroupK}
import cats.implicits._

import scala.collection.immutable.SortedMap

case class AccumMap[K, +E, +V](sortedMap: Nem[K, Either[E, V]]) {

  def map[B](f: V => B): AccumMap[K, E, B] =
    AccumMap(sortedMap.map(_.map(f)))

  def flatMap[EE >: E, B](f: V => AccumMap[K, EE, B])(implicit M: Monoid[K], O: Order[K]): AccumMap[K, EE, B] =
    AccumMap(
      sortedMap.toNel.flatMap { case (k, v) =>
        v.fold(e => Nel.one(k -> e.asLeft), f(_).sortedMap.toNel.map { case (k1, v1) => (k |+| k1) -> v1 })
      }.toNem)

  def combine[EE >: E, VV >: V: Semigroup](x: AccumMap[K, EE, VV]): AccumMap[K, EE, VV] =
    AccumMap(Nem.fromMapUnsafe(x.sortedMap.toSortedMap |+| sortedMap.toSortedMap))
}
object AccumMap {

  def apply[K: Order, V](kv: (K, V), kvs: (K, V)*): AccumMap[K, Nothing, V] =
    AccumMap(Nem(kv._1 -> kv._2.asRight, SortedMap(kvs.map(kv1 => kv1._1 -> kv1._2.asRight):_*)))

  def value[K]: ValuePartiallyApplied[K] = new ValuePartiallyApplied[K]

  class ValuePartiallyApplied[K] private[AccumMap] {
    def apply[V](value: V)(implicit M: Monoid[K], O: Order[K]): AccumMap[K, Nothing, V] =
      AccumMap(Nem.one(M.empty, value.asRight))
  }

  def either[K: Order, E, A](k: K, e: Either[E, A]): AccumMap[K, E, A] =
    AccumMap(Nem.one(k, e))


  implicit def monad[K: Order: Monoid, E]: Monad[AccumMap[K, E, *]] with SemigroupK[AccumMap[K, E, *]] =
    new Monad[AccumMap[K, E, *]] with SemigroupK[AccumMap[K, E, *]] {

      override def combineK[A](x: AccumMap[K, E, A], y: AccumMap[K, E, A]): AccumMap[K, E, A] =
        AccumMap(x.sortedMap ++ y.sortedMap)

      override def flatMap[A, B](fa: AccumMap[K, E, A])(f: A => AccumMap[K, E, B]): AccumMap[K, E, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => AccumMap[K, E, Either[A, B]]): AccumMap[K, E, B] =
        f(a).flatMap {
          case Left(value) => tailRecM(value)(f)
          case Right(value) => pure(value)
        }

      override def pure[A](x: A): AccumMap[K, E, A] =
        AccumMap.value[K](x)

      override def map[A, B](fa: AccumMap[K, E, A])(f: A => B): AccumMap[K, E, B] =
        fa.map(f)
    }
}
