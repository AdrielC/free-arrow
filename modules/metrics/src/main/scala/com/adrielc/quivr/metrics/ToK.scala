package com.adrielc.quivr.metrics

import cats.data.NonEmptyList
import simulacrum.{op, typeclass}

import cats.implicits._

@typeclass trait ToK[A] {

  @op("toK")
  def setK(a: A, k: Int): Option[A]
}

object ToK {

  implicit val toKReseults: ToK[NonEmptyList[Long]] = (a, k) => a.toList.take(k).toNel
}
