package com.adrielc

import cats.Show
import cats.arrow.{Arrow, ArrowChoice}
import cats.data.{Chain, Ior}
import cats.implicits._

package object quivr {

  type ~~>[-F[_, _], +G[_, _]] = BiFunctionK[F, G]

  type ToFunction[-F[_, _]] = BiFunctionK[F, Function1]
  object Pure {
    implicit def apply[F[a, b] <: a => b]: ToFunction[F] = new ToFunction[F] {
      def apply[A, B](fab: F[A, B]): A => B = fab
    }
  }

  /** A [[~~>]] that outputs values of kind "*" */
  type ~>|[-F[_, _], M] = F ~~> λ[(α, β) => M]

  def analyze[F[_, _]] = new AnalyzeOp[F]
  def analyze[F[_, _], A](f: F ~>| A) = new AnalyzeOp2(f)


  def liftA2[~>[_, _]: Arrow, A, B, C, D](ab: A ~> B)(ac: A ~> C)(f: (B, C) => D): A ~> D =
    (ab &&& ac) >>> Arrow[~>].lift(f.tupled)

  def test[~>[_, _]: Arrow, A](ab: A ~> Boolean): A ~> Either[A, A] =
    (Arrow[~>].id[A] &&& ab) >>> Arrow[~>].lift { case (a, b) => if(b) Left(a) else Right (a) }


  def ior[~>[_, _]: ArrowChoice, A, B, C, D](ab: A ~> B, cd: C ~> D): Ior[A, C] ~> Ior[B, D] =
    Arrow[~>].dimap(
      (ab +++ cd) +++ (ab *** cd)
    )((_: Ior[A, C]).fold(
      a => Left(Left(a)),
      b => Left(Right(b)),
      (a, b) => Right((a, b))
    ))({
      case Left(Left(a)) => Ior.left(a)
      case Left(Right(b)) => Ior.right(b)
      case Right((a, b)) => Ior.both(a, b)
    })
}

package quivr {

  class AnalyzeOp[F[_, _]] private[quivr] extends Serializable {
    def list: F ~>| List[F[_, _]] = BiFunctionK.collect[F]
    def chain: F ~>| Chain[F[_, _]] = BiFunctionK.id[F].pureK[Chain, F]
    def string(implicit S: Show[F[_, _]]): F ~>| String = new (F ~>| String) { def apply[A, B](fab: F[A, B]): String = S.show(fab) }
  }
  class AnalyzeOp2[F[_, _], A] private[quivr] (val f: F ~>| A) extends Serializable {
    def list: F ~>| List[A] = new (F ~>| List[A]) { def apply[C, B](fab: F[C, B]): List[A] = List(f(fab)) }
    def chain: F ~>| Chain[A] = new (F ~>| Chain[A]) { def apply[C, B](fab: F[C, B]): Chain[A] = Chain(f(fab)) }
  }
}
