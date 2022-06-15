package com.adrielc.quivr

import cats.arrow.Category
import shapeless.{HList, LabelledGeneric, MkRecordSelectLens, Witness}
import simulacrum.typeclass

@typeclass(excludeParents = "Category" :: Nil)
trait PipeOp[~>[_, _]] extends Category[~>] {

  def id[A]: A ~> A

  def swap[X, Y]: (X, Y) ~> (Y, X)

  def first[A, B, C](fa: ~>[A, B]): ~>[(A, C), (B, C)]

  def broadcast[A, B](fa: ~>[A, B]): ~>[A, (B, B)]

  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](f: ~>[B, C], g: ~>[A, B]): ~>[A, C]

  @simulacrum.op(">>>", alias = true)
  override def andThen[A, B, C](f: ~>[A, B], g: ~>[B, C]): ~>[A, C] =
    compose(g, f)


  def selectDynamic[A, R <: HList](k: Witness)
                                  (implicit
                                   G: LabelledGeneric.Aux[A, R],
                                   mkLens: MkRecordSelectLens[R, k.T]): ~>[A, mkLens.Elem]


  def second[A, B, C](fa: ~>[A, B]): ~>[(C, A), (C, B)] =
    compose(swap, compose(first[A, B, C](fa), swap))

  /**
   * Create a new computation `~>` that splits its input between `f` and `g`
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
   * `f` and `g` in the context of ~>. This means that `f *** g` may not be equivalent to `g *** f`.
   */
  @simulacrum.op("***", alias = true)
  def split[A, B, C, D](f: ~>[A, B], g: ~>[C, D]): ~>[(A, C), (B, D)] =
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
  @simulacrum.op("&&&", alias = true)
  def merge[A, B, C](f: ~>[A, B], g: ~>[A, C]): ~>[A, (B, C)] =
    andThen(broadcast(id[A]), split(f, g))
}

