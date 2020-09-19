package com.adrielc.quivr.free

import cats.arrow.Compose
import com.adrielc.quivr.data.{BiFunctionK, ~~>}

/**
 * Bare-bonez alternative to [[FreeArrow]]
  */
sealed abstract class FreeCompose[F[_, _], -A, +B] {
  import FreeCompose._

  final def foldMap[G[_, _], AA <: A, BB >: B](fg: F ~~> G)(implicit C: Compose[G]): G[AA, BB] = this match {
    case AndThen(AndThen(f, g), h) => (f >>> (g >>> h)).foldMap(fg)
    case AndThen(f, g) => C.andThen(f.foldMap(fg), g.foldMap(fg))
    case f: Lift[F, AA, BB] @unchecked => fg(f.f)
  }

  final def fold[AA <: A, BB >: B](implicit C: Compose[F]): F[AA, BB] = foldMap(BiFunctionK.id)

  def compose[Z](that: FreeCompose[F, Z, A]): FreeCompose[F, Z, B] =
    AndThen(that, this)

  def andThen[C](that: FreeCompose[F, B, C]): FreeCompose[F, A, C] =
    AndThen(this, that)

  def <<<[Z](that: FreeCompose[F, Z, A]): FreeCompose[F, Z, B] =
    this compose that

  def >>>[C](that: FreeCompose[F, B, C]): FreeCompose[F, A, C] =
    that compose this

  def >>^[C, BB >: B](f: F[BB, C]): FreeCompose[F, A, C] =
    AndThen(this, Lift(f))

  def <<^[C, AA <: A](f: F[C, AA]): FreeCompose[F, C, B] =
    AndThen(Lift(f), this)
}

object FreeCompose {
  def lift[F[_, _], A, B](f: F[A, B]): FreeCompose[F, A, B] = Lift(f)

  final private case class Lift[F[_, _], A, B](f: F[A, B]) extends FreeCompose[F, A, B]
  final private case class AndThen[F[_, _], A, B, C](f: FreeCompose[F, A, B], g: FreeCompose[F, B, C]) extends FreeCompose[F, A, C]

  implicit def category[F[_, _]]: Compose[FreeCompose[F, -*, +*]] = new Compose[FreeCompose[F, -*, +*]] {

    def compose[A, B, C](f: FreeCompose[F, B, C], g: FreeCompose[F, A, B]): FreeCompose[F, A, C] = AndThen(g, f)
  }
}
