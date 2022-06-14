package com.adrielc.quivr

import cats.arrow.Strong
import shapeless.{HList, LabelledGeneric, MkRecordSelectLens, Witness}
import simulacrum.{op, typeclass}

import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.ArrowLaws.
 */
@implicitNotFound("Could not find an instance of Arrow for ${F}")
@typeclass(excludeParents = "Pipe" :: "Strong" :: Nil)
trait Arrow[F[_, _]] extends Pipe[F] with Strong[F] { self =>

  /**
   *  Lift a function into the context of an Arrow.
   *
   * In the reference articles "Arrows are Promiscuous...", and in the corresponding Haskell
   * library `Control.Arrow`, this function is called `arr`.
   */
  def lift[A, B](f: A => B): F[A, B]

  override def id[A]: F[A, A] = lift(identity)

  @op("dimap", alias = false)
  override def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  override def swap[X, Y]: F[(X, Y), (Y, X)] = rmap(id[(X, Y)])(_.swap)

  override def broadcast[A, B](fa: F[A, B]): F[A, (B, B)] = rmap(fa)(a => (a, a))

  override def selectDynamic[A, R <: HList](k: Witness)
                                           (implicit
                                            G: LabelledGeneric.Aux[A, R],
                                            mkLens: MkRecordSelectLens[R, k.T]): F[A, mkLens.Elem] =
    rmap(lift[A, R](G.to(_: A)))(mkLens().get(_: R))
}

object Arrow {

  implicit def fromCatsArrow[F[_, _]: cats.arrow.Arrow]: Arrow[F] = new AR[F] {
    lazy val ac = cats.arrow.Arrow[F]

    /**
     * ArrowChoice yields Arrows with choice, allowing distribution
     * over coproducts.
     *
     * Given two `F`s (`f` and `g`), create a new `F` with
     * domain the coproduct of the domains of `f` and `g`,
     * and codomain the coproduct of the codomains of `f` and `g`.
     * This is the sum notion to `split`'s product.
     *
     * Example:
     * {{{
     * scala> import cats.implicits._
     * scala> val toLong: Int => Long = _.toLong
     * scala> val toDouble: Float => Double = _.toDouble
     * scala> val f: Either[Int, Float] => Either[Long, Double] = toLong +++ toDouble
     * scala> f(Left(3))
     * res0: Either[Long,Double] = Left(3)
     * scala> f(Right(3))
     * res1: Either[Long,Double] = Right(3.0)
     * }}}
     */

    override def lift[A, B](f: A => B): F[A, B] = ac.lift(f)

    override def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = ac.first(fa)

    override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = ac.compose(f, g)

    override def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] = ac.andThen(f, g)

    override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = ac.second(fa)

    override def id[A]: F[A, A] = ac.id
  }
}