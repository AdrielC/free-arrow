package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import cats.data.NonEmptyMap
import simulacrum.typeclass

@typeclass trait ResultLabels[A] extends Serializable {

  def resultLabels(a: A): NonEmptyMap[ResultId, Label]
}
object ResultLabels {

  implicit val resultLabelsIdentityInstance: ResultLabels[NonEmptyMap[ResultId, Label]] = identity

  implicit def resultLabelsRTuple[A, B](implicit RB: ResultLabels[B]): ResultLabels[(A, B)] = contravariantRes.contramap(RB)(_._2)

  implicit val contravariantRes: Contravariant[ResultLabels] = new Contravariant[ResultLabels] {
    def contramap[A, B](fa: ResultLabels[A])(f: B => A): ResultLabels[B] = a => fa.resultLabels(f(a))
  }
}
