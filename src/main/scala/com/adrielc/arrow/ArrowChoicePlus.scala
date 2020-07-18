package com.adrielc.arrow

import simulacrum.{op, typeclass}
import cats.syntax.either._

@typeclass trait ArrowChoicePlus[~>[_, _]] extends ArrowChoiceZero[~>] with ArrowPlus[~>] {

  @op("|&|", alias = true)
  def and[A, B, C](f: A ~> B, g: A ~> C): A ~> Either[B, C] =
    andThen(plus(lift(_.asLeft[A]), lift(_.asRight[A])), choose(f)(g))
}

object ArrowChoicePlus {

  implicit val partialFunction: ArrowChoicePlus[PartialFunction] = new ArrowChoicePlus[PartialFunction] {

    override def choose[A, B, C, D](f: PartialFunction[A, C])(g: PartialFunction[B, D]): PartialFunction[Either[A, B], Either[C, D]] =
      new PartialFunction[Either[A, B], Either[C, D]] {
        def isDefinedAt(x: Either[A, B]): Boolean = x.fold(f.isDefinedAt, g.isDefinedAt)
        def apply(v1: Either[A, B]): Either[C, D] = v1.bimap(f, g)
      }

    override def plus[A, B](f: PartialFunction[A, B], g: PartialFunction[A, B]): PartialFunction[A, B] = f orElse g

    override def zeroArrow[B, C]: PartialFunction[B, C] = PartialFunction.empty[B, C]

    override def lift[A, B](f: A => B): PartialFunction[A, B] = PartialFunction(f)

    override def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]): PartialFunction[A, C] = new PartialFunction[A, C] {
      def isDefinedAt(x: A): Boolean = g.isDefinedAt(x) && f.isDefinedAt(g(x))
      def apply(v1: A): C = f.compose(g)(v1)
    }

    override def first[A, B, C](fa: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = new PartialFunction[(A, C), (B, C)] {
      override def isDefinedAt(x: (A, C)): Boolean = fa.isDefinedAt(x._1)
      override def apply(v1: (A, C)): (B, C) = (fa(v1._1), v1._2)
    }

  }

}
