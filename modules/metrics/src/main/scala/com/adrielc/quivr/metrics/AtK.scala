package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap}
import simulacrum.{op, typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Rank
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._

@typeclass trait AtK[A] {

  @op("atK")
  def atK(a: A, k: Rank): Option[A]
}

object AtK {

  implicit def toKReseults[A]: AtK[NonEmptyList[A]] =
    (a, k) => if(k > a.length) none else a.toList.take(k).toNel

  implicit def toKIndexes[A]: AtK[NonEmptyMap[Rank, A]] =
    (a, k) => a.toNel.filter(_._1 <= k).toNel.map(_.toNem)
}