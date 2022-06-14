package com.adrielc.quivr


import cats.arrow.Choice
import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ArrowChoiceLaws.
 */
@typeclass(excludeParents = "Arrow" :: "Choice" :: Nil)
trait ArrowChoice[F[_, _]] extends Arrow[F] with Choice[F] { self =>

  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  override def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)


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
  @simulacrum.op("+++", alias = true)
  def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]

  def left[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]] =
    choose(fab)(lift(identity[C]))

  def right[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]] =
    choose(lift(identity[C]))(fab)

  override def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] =
    rmap(choose(f)(g))(_.fold(identity, identity))


  /**
   * Create a new computation `F` that splits its input between `f` and `g`
   * and combines the output of each.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val toDouble: Float => Double = _.toDouble
   * scala> val f: ((Int, Float)) => (Long, Double) = Arrow[Function1].split(toLong, toDouble)
   * scala> f((3, 4.0f))
   * res0: (Long, Double) = (3,4.0)
   * }}}
   *
   * Note that the arrow laws do not guarantee the non-interference between the _effects_ of
   * `f` and `g` in the context of F. This means that `f *** g` may not be equivalent to `g *** f`.
   */

  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))

  /**
   * Create a new computation `F` that merge outputs of `f` and `g` both having the same input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val addEmpty: Int => Int = _ + 0
   * scala> val multiplyEmpty: Int => Double= _ * 1d
   * scala> val f: Int => (Int, Double) = addEmpty &&& multiplyEmpty
   * scala> f(1)
   * res0: (Int, Double) = (1,1.0)
   * }}}
   *
   * Note that the arrow laws do not guarantee the non-interference between the _effects_ of
   *  `f` and `g` in the context of F. This means that `f &&& g` may not be equivalent to `g &&& f`.
   */

  override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] =
    andThen(broadcast(id[A]), split(f, g))
}


object ArrowChoice {

  implicit def fromCatsArrowChoice[F[_, _]: cats.arrow.ArrowChoice]: AC[F] =
    new instances.ComposedArrowChoiceInstance[F] with AC[F] {
    lazy val A = cats.arrow.ArrowChoice[F]
  }
}