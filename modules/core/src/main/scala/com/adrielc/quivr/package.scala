package com.adrielc

import cats.data.Ior
import com.adrielc.quivr.implicits._

package object quivr {

  /** Arrow hierarchy supported by FreeArrow */
  type AR[f[_, _]] = Arrow[f]
  type PP[f[_, _]] = Pipe[f]
  type AC[f[_, _]] = ArrowChoice[f]
  type AP[f[_, _]] = ArrowPlus[f]
  type AZ[f[_, _]] = ArrowZero[f]
  type ACZ[f[_, _]] = ArrowChoiceZero[f]
  type ACP[f[_, _]] = ArrowChoicePlus[f]

  type ~~>[-F[_, _], +G[_, _]] = BiFunctionK[F, G]

  /** [[quivr.BiInjectK]][F, G] */
  type :<<:[F[_, _], G[_, _]] = BiInjectK[F, G]


  type Pure[-F[_, _]] = BiFunctionK[F, Function1]
  object Pure {
    implicit def apply[F[a, b] <: a => b]: Pure[F] = new Pure[F] {
      def apply[A, B](fab: F[A, B]): A => B = fab
    }
  }

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
          with ArrowChoice.ToArrowChoiceOps
          with Arrow.ToArrowOps
          with Pipe.ToPipeOps
  }

  object implicits
    extends syntax.AllSyntax
      with instances.AllInstances


  def liftA2[~>[_, _]: Arrow, A, B, C, D](ab: A ~> B)(ac: A ~> C)(f: (B, C) => D): A ~> D =
    Arrow[~>].andThen((ab &&& ac), Arrow[~>].lift(f.tupled))

  def test[~>[_, _]: Arrow, A](ab: A ~> Boolean): A ~> Either[A, A] =
    Arrow[~>].andThen(Arrow[~>].id[A] merge ab, Arrow[~>].lift[(A, Boolean), Either[A, A]] {
      case (a, b) => if(b) Left(a) else Right (a) })

  def ior[~>[_, _]: ArrowChoice, A, B, C, D](ab: A ~> B, cd: C ~> D): (A Ior C) ~> (B Ior D) =
    ArrowChoice[~>].dimap((ab +++ cd) +++ (ab *** cd))(iorToEither[A, C])(eitherToIor)

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
