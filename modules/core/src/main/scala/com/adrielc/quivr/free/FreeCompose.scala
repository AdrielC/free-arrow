package com.adrielc.quivr.free

import cats.Eval
import cats.arrow.Compose
import com.adrielc.quivr.data.{BiFunctionK, ~~>}
import cats.implicits._

/**
 * Bare-bonez alternative to [[FreeArrow]]
  */
sealed abstract class FreeCompose[F[_, _], A, B] {
  import FreeCompose._

  /**
   * Stack safe if compose on [[G]] is stack safe
    */
  final def foldMap[G[_, _]](fg: F ~~> G)(implicit C: Compose[G]): G[A, B] = {

    lazy val lazyAnd = new (FreeCompose[F, *, *] ~~> λ[(α, β) => Eval[G[α, β]]]) {
      def apply[D, E](f: FreeCompose[F, D, E]): Eval[G[D, E]] = f match {
        case a: AndThen[F, D, b, E] =>

          for {
            b <- Eval.later(apply(a.f)).flatten
            e <- apply(a.g)
          } yield b >>> e

        case _ => Eval.now(f.foldMap(fg))
      }
    }

    this match {

      case AndThen(AndThen(f, g), h) => (f >>> (g >>> h)).foldMap(fg)

      case andThen: AndThen[F, A, b, B] @unchecked => (for {
          e <- lazyAnd(andThen.f)
          b <- Eval.later(lazyAnd(andThen.g)).flatten
      } yield e >>> b).value

      case LiftK(f) => fg(f)
    }
  }

  final def fold(implicit C: Compose[F]): F[A, B] = foldMap(BiFunctionK.id)

  def compose[Z](that: FreeCompose[F, Z, A]): FreeCompose[F, Z, B] =
    AndThen(that, this)

  def andThen[C](that: FreeCompose[F, B, C]): FreeCompose[F, A, C] =
    AndThen(this, that)

  def <<<[Z](that: FreeCompose[F, Z, A]): FreeCompose[F, Z, B] =
    this compose that

  def >>>[C](that: FreeCompose[F, B, C]): FreeCompose[F, A, C] =
    that compose this

  def >>^[C](f: F[B, C]): FreeCompose[F, A, C] =
    AndThen(this, LiftK(f))

  def <<^[C](f: F[C, A]): FreeCompose[F, C, B] =
    AndThen(LiftK(f), this)
}

object FreeCompose {
  def liftK[F[_, _], A, B](f: F[A, B]): FreeCompose[F, A, B] = LiftK(f)

  final private case class LiftK[F[_, _], A, B](f: F[A, B]) extends FreeCompose[F, A, B]
  final private case class AndThen[F[_, _], A, B, C](f: FreeCompose[F, A, B], g: FreeCompose[F, B, C]) extends FreeCompose[F, A, C]

  implicit def category[F[_, _]]: Compose[FreeCompose[F, *, *]] = new Compose[FreeCompose[F, *, *]] {

    def compose[A, B, C](f: FreeCompose[F, B, C], g: FreeCompose[F, A, B]): FreeCompose[F, A, C] = AndThen(g, f)
  }
}
