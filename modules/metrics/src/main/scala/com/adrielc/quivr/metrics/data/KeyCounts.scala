package com.adrielc.quivr.metrics
package data

import cats.data.{NonEmptyMap, NonEmptyList}
import cats.kernel.{CommutativeSemigroup, Order}
import cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._

case class KeyCounts[K](counts: NonEmptyMap[K, NonZeroCount]) {

  def +(other: KeyCounts[K])(implicit O: Order[K]): KeyCounts[K] =
    KeyCounts(NonEmptyMap.fromMapUnsafe(counts.toSortedMap |+| other.counts.toSortedMap))

  def toMap: Map[K, Int] = counts.toSortedMap.mapValues(_.value)

  def binarize: KeyCounts[K] =
    copy(counts = counts.map(_ => 1))
}
object KeyCounts {

  def fromMap[K: Order](map: Map[K, Int]): Option[KeyCounts[K]] = for {
    nel       <- NonEmptyList.fromList(map.toList)
    posCount  <- nel.traverse(_.traverse(NonZeroCount.from(_).toOption))
  } yield KeyCounts(posCount.toNem)

  implicit def semigroup[E: Order]: CommutativeSemigroup[KeyCounts[E]] =
    CommutativeSemigroup.instance(_ + _)
}