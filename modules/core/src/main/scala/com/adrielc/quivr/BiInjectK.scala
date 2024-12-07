package com.adrielc.quivr

import cats.instances.option._
import com.adrielc.quivr.data.BiEitherK

abstract class BiInjectK[F[_, _], G[_, _]] {

  def inj: BiFunctionK[F, G]

  def prj: BiFunctionK[G, λ[(α, β) => Option[F[α, β]]]]

  final def apply[A, B](fab: F[A, B]): G[A, B] = inj(fab)

  final def unapply[A, B](ga: G[A, B]): Option[F[A, B]] = prj(ga)
}

sealed abstract private[quivr] class BiInjectKInstances {
  implicit def reflexiveBiInjectKInstance[F[_, _]]: BiInjectK[F, F] =
    new BiInjectK[F, F] {
      val inj = BiFunctionK.id

      val prj = BiFunctionK.pure.lift
    }

  implicit def leftBiInjectKInstance[F[_, _], G[_, _]]: BiInjectK[F, BiEitherK[F, G, *, *]] =
    new BiInjectK[F, BiEitherK[F, G, *, *]] {
      val inj = BiEitherK.leftK

      val prj = new (BiEitherK[F, G, *, *] ~~> λ[(α, β) => Option[F[α, β]]]) {
        def apply[A, B](fab: BiEitherK[F, G, A, B]): Option[F[A, B]] = fab.run.left.toOption
      }
    }

  implicit def rightBiInjectKInstance[F[_, _], G[_, _], H[_, _]](implicit I: BiInjectK[F, G]): BiInjectK[F, BiEitherK[H, G, *, *]] =
    new BiInjectK[F, BiEitherK[H, G, *, *]] {
      val inj = BiEitherK.rightK.compose(I.inj)

      val prj = new (BiEitherK[H, G, *, *] ~~> λ[(α, β) => Option[F[α, β]]]) {
        def apply[A, B](fab: BiEitherK[H, G, A, B]): Option[F[A, B]] = fab.run.toOption.flatMap(I.prj(_))
      }
    }
}

object BiInjectK extends BiInjectKInstances {
  def apply[F[_, _], G[_, _]](implicit I: BiInjectK[F, G]): BiInjectK[F, G] = I
}