package com.adrielc.quivr.data

import cats.data.{NonEmptyList => Nel, NonEmptyMap}
import cats.{Monad, Monoid, Order, Semigroup, SemigroupK}
import cats.implicits._

import scala.collection.immutable.SortedMap

/**
 * A non empty map that represents a store of values with monoidal keys
 *
 * @param nem Non empty map
 * @tparam K
 * @tparam E
 * @tparam V
 */
case class AccumMap[K, +E, +V](nem: NonEmptyMap[K, Either[E, V]]) {

  def map[B](f: V => B): AccumMap[K, E, B] =
    AccumMap(nem.map(_.map(f)))

  def flatMap[EE >: E, B](f: V => AccumMap[K, EE, B])(implicit S: Semigroup[K], O: Order[K]): AccumMap[K, EE, B] =
    AccumMap(
      nem.toNel.flatMap { case (k, v) =>
        v.fold(e => Nel.one(k -> e.asLeft), f(_).nem.toNel.map { case (k1, v1) => (k |+| k1) -> v1 })
      }.toNem)

  def combine[EE >: E, VV >: V: Semigroup](x: AccumMap[K, EE, VV])(implicit O: Order[K]): AccumMap[K, EE, VV] =
    AccumMap(NonEmptyMap.fromMapUnsafe(x.nem.toSortedMap |+| nem.toSortedMap))
}
object AccumMap {

  def apply[K: Order, V](kv: (K, V), kvs: (K, V)*): AccumMap[K, Nothing, V] =
    AccumMap(NonEmptyMap(kv._1 -> kv._2.asRight, SortedMap(kvs.map(kv1 => kv1._1 -> kv1._2.asRight):_*)))

  def value[K]: ValuePartiallyApplied[K] = new ValuePartiallyApplied[K]

  class ValuePartiallyApplied[K] private[AccumMap] {
    def apply[V](value: V)(implicit M: Monoid[K], O: Order[K]): AccumMap[K, Nothing, V] =
      AccumMap(NonEmptyMap.one(M.empty, value.asRight))
  }

  def either[K: Order, E, A](k: K, e: Either[E, A]): AccumMap[K, E, A] =
    AccumMap(NonEmptyMap.one(k, e))


  implicit def monad[K: Order: Monoid, E]: Monad[AccumMap[K, E, *]] with SemigroupK[AccumMap[K, E, *]] =
    new Monad[AccumMap[K, E, *]] with SemigroupK[AccumMap[K, E, *]] {

      override def combineK[A](x: AccumMap[K, E, A], y: AccumMap[K, E, A]): AccumMap[K, E, A] =
        AccumMap(x.nem ++ y.nem)

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
