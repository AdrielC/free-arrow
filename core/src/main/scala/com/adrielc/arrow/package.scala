package com.adrielc

import cats.arrow.{Arrow, ArrowChoice}
import cats.data.Ior
import cats.syntax.all._

package object arrow {

  type ~~>[-F[_, _], +G[_, _]] = BiFunctionK[F, G]

  type Pure[-F[_, _]] = BiFunctionK[F, Function1]

  /** A [[~~>]] that outputs values of kind "*" */
  type ~>|[-F[_, _], +M] = F ~~> λ[(α, β) => M]

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
