package com.adrielc.quivr.util

trait BiDistributes[F[_, _], G[_, _]] {

  def dist[A0, A1, B0, B1](pa: F[A0, A1], pb: F[B0, B1]): F[G[A0, B0], G[A1, B1]]
}

object BiDistributes {

  def apply[F[_, _], G[_, _]](implicit D: BiDistributes[F, G]): BiDistributes[F, G] = D
}

