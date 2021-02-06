package com.adrielc

import cats.arrow.{Arrow, ArrowChoice}
import cats.data.Ior
import cats.implicits._

package object quivr {

  /** Arrow hierarchy supported by FreeArrow */
  type AR[f[_, _]] = Arrow[f]
  type AC[f[_, _]] = ArrowChoice[f]
  type AP[f[_, _]] = ArrowPlus[f]
  type AZ[f[_, _]] = ArrowZero[f]
  type ACZ[f[_, _]] = ArrowChoiceZero[f]
  type ACP[f[_, _]] = ArrowChoicePlus[f]

  type ~~>[-F[_, _], +G[_, _]] = BiFunctionK[F, G]

  /** [[quivr.BiInjectK]][F, G] */
  type :<<:[F[_, _], G[_, _]] = BiInjectK[F, G]


  type Pure[-F[_, _]] = BiFunctionK.Pure[F]

  type EitherIor[A, B] = Either[Either[A, B], (A, B)]

  /** A [[~~>]] that outputs values of kind "*" */
  type ~>|[-F[_, _], M] = F ~~> λ[(α, β) => M]

  object syntax {

    object all extends AllSyntax

    trait AllSyntax
      extends ArrowChoicePlus.ToArrowChoicePlusOps
        with ArrowChoiceZero.ToArrowChoiceZeroOps
        with ArrowPlus.ToArrowPlusOps
        with ArrowZero.ToArrowZeroOps
  }

  object implicits
    extends syntax.AllSyntax
      with instances.AllInstances


  def liftA2[~>[_, _]: Arrow, A, B, C, D](ab: A ~> B)(ac: A ~> C)(f: (B, C) => D): A ~> D =
    (ab &&& ac) >>> Arrow[~>].lift(f.tupled)

  def test[~>[_, _]: Arrow, A](ab: A ~> Boolean): A ~> Either[A, A] =
    (Arrow[~>].id[A] &&& ab) >>> Arrow[~>].lift { case (a, b) => if(b) Left(a) else Right (a) }

  def ior[~>[_, _]: ArrowChoice, A, B, C, D](ab: A ~> B, cd: C ~> D): (A Ior C) ~> (B Ior D) =
    ((ab +++ cd) +++ (ab *** cd)).dimap(iorToEither[A, C])(eitherToIor)

  def eitherToIor[A, B](e: EitherIor[A, B]): Ior[A, B] = e match {
    case Left(Left(a)) => Ior.left(a)
    case Left(Right(b)) => Ior.right(b)
    case Right((a, b)) => Ior.both(a, b)
  }

  def iorToEither[A, B](e: Ior[A, B]): EitherIor[A, B] = e match {
    case Ior.Left(a)    => Left(Left(a))
    case Ior.Right(b)   => Left(Right(b))
    case Ior.Both(a, b) => Right((a, b))
  }

  val iorToEitherK: Ior ~~> EitherIor = BiFunctionK.lift(iorToEither)

  val eitherToIorK: EitherIor ~~> Ior = BiFunctionK.lift[EitherIor, Ior](eitherToIor)
}
