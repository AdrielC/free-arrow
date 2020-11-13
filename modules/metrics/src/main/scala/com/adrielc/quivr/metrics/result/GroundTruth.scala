package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import cats.data.NonEmptySet
import com.adrielc.quivr.metrics.retrieval.GroundTruthCount
import simulacrum.typeclass

@typeclass trait GroundTruth[A] extends GroundTruthCount[A] {

  def groundTruth(a: A): GroundTruth.Set

  override def groundTruthCount(a: A): Int =
    groundTruth(a).nRel
}
object GroundTruth {

  // relevant document set
  case class Set(set: NonEmptySet[ResultId]) {

    lazy val nRel: Int = set.length
  }

  implicit val contravariantGroundTruthSet: Contravariant[GroundTruth] = new Contravariant[GroundTruth] {
    def contramap[A, B](fa: GroundTruth[A])(f: B => A): GroundTruth[B] = a => fa.groundTruth(f(a))
  }
}