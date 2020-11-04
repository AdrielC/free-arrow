package com.adrielc

import cats.arrow.{Arrow, ArrowChoice}
import cats.data.Ior
import cats.implicits._

package object quivr {

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


  type ToFunction[-F[_, _]] = BiFunctionK[F, Function1]
  object Pure {
    implicit def apply[F[a, b] <: a => b]: ToFunction[F] = new ToFunction[F] {
      def apply[A, B](fab: F[A, B]): A => B = fab
    }
  }

  type EitherIor[A, B] = Either[Either[A, B], (A, B)]

  /** A [[~~>]] that outputs values of kind "*" */
  type ~>|[-F[_, _], M] = F ~~> λ[(α, β) => M]

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

package quivr {

  private[quivr] trait ComposedArrowInstance[F[_, _]] extends Arrow[F] {

    def A: Arrow[F]

    def lift[A, B](f: A => B): F[A, B] = A.lift(f)
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = A.compose(f, g)
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = A.first(fa)
    override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = A.second(fa)
    override def id[A]: F[A, A] = A.id
    override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = A.split(f, g)
    override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = A.merge(f, g)
    override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = A.rmap(fab)(f)
    override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] = A.lmap(fab)(f)
  }

  private[quivr] trait ComposedArrowChoiceInstance[~>[_, _]] extends ComposedArrowInstance[~>] with ArrowChoice[~>] {

    def A: ArrowChoice[~>]

    def choose[A, B, C, D](f: A ~> C)(g: B ~> D): Either[A, B] ~> Either[C, D] = A.choose(f)(g)
    override def choice[A, B, C](f: A ~> C, g: B ~> C): Either[A, B] ~> C = A.choice(f, g)
    override def left[A, B, C](fab: A ~> B): Either[A, C] ~> Either[B, C] = A.left(fab)
    override def right[A, B, C](fab: A ~> B): Either[C, A] ~> Either[C, B] = A.right(fab)
  }
}
