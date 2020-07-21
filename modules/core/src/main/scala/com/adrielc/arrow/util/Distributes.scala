package com.adrielc.arrow.util

trait Distributes[F[_, _], G[_, _]] {

  def dist[A0, A1, B0, B1](pa: F[A0, A1], pb: F[B0, B1]): F[G[A0, B0], G[A1, B1]]
}

object Distributes {

  def apply[F[_, _], G[_, _]](implicit D: Distributes[F, G]): Distributes[F, G] = D
}