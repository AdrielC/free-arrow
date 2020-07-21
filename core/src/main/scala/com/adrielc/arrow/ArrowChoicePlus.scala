package com.adrielc.arrow

import cats.arrow.ArrowChoice
import cats.data.Kleisli
import cats.{Monad, MonoidK}
import simulacrum.{op, typeclass}
import cats.syntax.all._

@typeclass trait ArrowChoicePlus[~>[_, _]] extends ArrowChoiceZero[~>] with ArrowPlus[~>] {

  @op("|&|", alias = true)
  def and[A, B, C](f: A ~> B, g: A ~> C): A ~> Either[B, C] =
    andThen(plus(lift(_.asLeft[A]), lift(_.asRight[A])), choose(f)(g))
}


object ArrowChoicePlus {

  implicit def kleisliACP[M[_] : Monad : MonoidK]: ArrowChoicePlus[Kleisli[M, ?, ?]] =
    new ArrowChoicePlusInstance[Kleisli[M, ?, ?]] {

      val A: ArrowChoice[Kleisli[M, ?, ?]] = Kleisli.catsDataArrowChoiceForKleisli

      def plus[A, B](f: Kleisli[M, A, B], g: Kleisli[M, A, B]): Kleisli[M, A, B] =
        Kleisli(a => f.run(a) <+> g.run(a))

      def zeroArrow[B, C]: Kleisli[M, B, C] =
        Kleisli.liftF(MonoidK[M].empty)
    }
}

private[arrow] trait ArrowChoicePlusInstance[~>[_, _]] extends ArrowChoicePlus[~>] {

  def A: ArrowChoice[~>]

  def compose[A, B, C](f: B ~> C, g: A ~> B): A ~> C =
    A.compose(f, g)

  def lift[A, B](f: A => B): A ~> B =
    A.lift(f)

  def first[A, B, C](fa: A ~> B): (A, C) ~> (B, C) =
    A.first(fa)

  def choose[A, B, C, D](f: A ~> C)(g: B ~> D): Either[A, B] ~> Either[C, D] =
    A.choose(f)(g)

  override def split[A, B, C, D](f: A ~> B, g: C ~> D): (A, C) ~> (B, D) = A.split(f, g)
}