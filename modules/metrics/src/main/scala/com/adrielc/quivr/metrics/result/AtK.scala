package com.adrielc.quivr.metrics.result

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits.none
import simulacrum.{op, typeclass}

@typeclass trait AtK[A] {

  @op("atK")
  def atK(a: A, k: Int): Option[A]
}

object AtK {

  implicit def toKReseults[A]: AtK[NonEmptyList[A]] =
    (a, k) => if(k > a.length) none else NonEmptyList.fromList(a.toList.take(k))

  implicit def toKVectorReseults[A]: AtK[NonEmptyVector[A]] =
    (a, k) => if(k > a.length) none else NonEmptyVector.fromVector(a.toVector.take(k))

  implicit def toLeftTupleAtK[A: AtK, B]: AtK[(A, B)] = (ab, k) => AtK[A].atK(ab._1, k).map(_ -> ab._2)
}
