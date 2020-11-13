package com.adrielc.quivr.metrics
package result

import cats.Contravariant
import simulacrum.typeclass

@typeclass trait Relevancy[@specialized Rel] {

  def gain(r: Rel): Gain
}

object Relevancy {

  implicit def relevancyOpt[A: Relevancy]: Relevancy[Option[A]] = _.map(Relevancy[A].gain).getOrElse(0.0)

  implicit def relevancyNum[N](implicit N: Numeric[N]): Relevancy[N] = N.toDouble(_)

  implicit val contravariant: Contravariant[Relevancy] = new Contravariant[Relevancy] {
    def contramap[A, B](fa: Relevancy[A])(f: B => A): Relevancy[B] = a => fa.gain(f(a))
  }
}

@typeclass trait BinaryRelevancy[@specialized(Boolean) Rel] {

  def isRel(a: Rel): Boolean
}

object BinaryRelevancy {

  implicit val fromBool: BinaryRelevancy[Boolean] = b => {
    println("fromBool\t\t\t\t")
    b
  }

  implicit def fromOpt[A: BinaryRelevancy]: BinaryRelevancy[Option[A]] = a => {
    println("fromBool\t\t\t\t")
    a.exists(BinaryRelevancy[A].isRel)
  }

  implicit val contravariant: Contravariant[BinaryRelevancy] = new Contravariant[BinaryRelevancy] {
    def contramap[A, B](fa: BinaryRelevancy[A])(f: B => A): BinaryRelevancy[B] = a => fa.isRel(f(a))
  }

  implicit def fromRelAndBinary[A: Relevancy, B: BinaryRelevancy]: BinaryRelevancy[(A, B)] with Relevancy[(A, B)] =
    new BinaryRelevancy[(A, B)] with Relevancy[(A, B)] {
      override def isRel(a: (A, B)): Boolean = BinaryRelevancy[B].isRel(a._2)
      override def gain(r: (A, B)): Gain = Relevancy[A].gain(r._1)
    }
}
