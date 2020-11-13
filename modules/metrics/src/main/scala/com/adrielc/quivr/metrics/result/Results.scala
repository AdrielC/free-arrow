package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyVector}
import com.adrielc.quivr.metrics.retrieval.ResultCount
import simulacrum.{op, typeclass}

@typeclass trait Results[A] extends ResultCount[A] {

  @op("results")
  def results(a: A): NonEmptyVector[ResultId]

  override def resultCount(a: A): Int =
    results(a).length
}
object Results {

  implicit val resultSetIdentityInstance: Results[NonEmptyVector[ResultId]] = identity

  implicit val resultSetNelInstance: Results[NonEmptyList[ResultId]] = nel => NonEmptyVector(nel.head, nel.tail.toVector)

  implicit def resultsLeftTuple[A: Results, B]: Results[(A, B)] = a => Results[A].results(a._1)

  implicit val contravariantResultSet: Contravariant[Results] = new Contravariant[Results] {
    def contramap[A, B](fa: Results[A])(f: B => A): Results[B] = a => fa.results(f(a))
  }
}
