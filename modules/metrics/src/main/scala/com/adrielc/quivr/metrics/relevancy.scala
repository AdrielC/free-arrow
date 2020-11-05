package com.adrielc.quivr.metrics

import cats.Contravariant
import simulacrum.typeclass
import cats.syntax.all._
import com.adrielc.quivr.metrics.data._
import com.adrielc.quivr.metrics.data.relevance.{Relevance, SABRel}

import Numeric.Implicits._

object relevancy {

  @typeclass trait Relevancy[Rel] {

    def gainValue(r: Rel): Option[Gain]

    def isJudged(a: Rel): Boolean = gainValue(a).isDefined
    def isRel(a: Rel): Boolean = gainValue(a).exists(_.value > 0)
    def gainOrZero(r: Rel): Gain = gainValue(r).getOrElse(Gain.zero)
  }
  object Relevancy {
    implicit val gainRelevancy: Relevancy[Option[Gain]] = identity

    implicit val relevancyOptBool: Relevancy[Option[Boolean]] = _.map(if(_) Gain.one else Gain.zero)

    implicit def relevancyOptNum[N: Numeric]: Relevancy[Option[N]] = _.flatMap(n => Gain.from(n.toDouble).toOption)

    implicit def relevancySABRel: Relevancy[SABRel] = _.gain

    implicit val relevanceRelevancy: Relevancy[Relevance] = _.gain

    implicit def relevancyNonOpt[A](implicit R: Relevancy[Option[A]]): Relevancy[A] = R.contramap(Option(_))

    implicit val contravariant: Contravariant[Relevancy] = new Contravariant[Relevancy] {
      def contramap[A, B](fa: Relevancy[A])(f: B => A): Relevancy[B] = a => fa.gainValue(f(a))
    }
  }
}
