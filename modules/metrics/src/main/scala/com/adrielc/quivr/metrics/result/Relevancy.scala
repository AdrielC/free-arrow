package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import com.adrielc.quivr.metrics.data.relevance.{Relevance, SABN}
import simulacrum.typeclass

@typeclass trait Relevancy[@specialized(Double, Int, Boolean) Rel] {
  def gainValue(r: Rel): Option[Gain]

  def isJudged(a: Rel): Boolean = gainValue(a).isDefined

  def isRel(a: Rel): Boolean = gainValue(a).exists(_ > 0)

  def gainOrZero(r: Rel): Double = gainValue(r).getOrElse(0.0)
}

object Relevancy {

  implicit val gainRelevancy: Relevancy[Option[Gain]] = identity

  implicit val relevancyOptBool: Relevancy[Option[Boolean]] = _.flatMap(if (_) Some(1.0) else None)

  implicit def relevancyOptNum[N](N: Numeric[N]): Relevancy[Option[N]] = _.map(N.toDouble)

  implicit def relevancySABRel: Relevancy[SABN] = _.gain

  implicit val relevanceRelevancy: Relevancy[Relevance] = _.gain

  implicit def relevancyNonOpt[A](implicit R: Relevancy[Option[A]]): Relevancy[A] = contravariant.contramap(R)(Option(_))

  implicit val contravariant: Contravariant[Relevancy] = new Contravariant[Relevancy] {
    def contramap[A, B](fa: Relevancy[A])(f: B => A): Relevancy[B] = a => fa.gainValue(f(a))
  }
}