package com.adrielc.arrow.data

import cats.arrow.ArrowChoice
import cats.syntax.arrowChoice._
import cats.syntax.choice._
import cats.syntax.compose._
import cats.syntax.strong._


case class Tuple2P[F[_, _], G[_, _], A, B](_1: F[A, B], _2: G[A, B])

object Tuple2P {

  implicit def tuple2HKArrow[F[_, _], G[_, _]]
  (implicit F: ArrowChoice[F], G: ArrowChoice[G]): ArrowChoice[Tuple2P[F, G, ?, ?]] =
    new ArrowChoice[Tuple2P[F, G, ?, ?]] {

      def choose[A, B, C, D](f: Tuple2P[F, G, A, C])(g: Tuple2P[F, G, B, D]): Tuple2P[F, G, Either[A, B], Either[C, D]] =
        Tuple2P(f._1 +++ g._1, f._2 +++ g._2)

      def lift[A, B](f: A => B): Tuple2P[F, G, A, B] =
        Tuple2P(F.lift(f), G.lift(f))

      def compose[A, B, C](f: Tuple2P[F, G, B, C], g: Tuple2P[F, G, A, B]): Tuple2P[F, G, A, C] =
        Tuple2P(g._1 >>> f._1, g._2 >>> f._2)

      def first[A, B, C](fa: Tuple2P[F, G, A, B]): Tuple2P[F, G, (A, C), (B, C)] =
        Tuple2P(fa._1.first, fa._2.first)

      override def second[A, B, C](fa: Tuple2P[F, G, A, B]): Tuple2P[F, G, (C, A), (C, B)] =
        Tuple2P(fa._1.second, fa._2.second)

      override def left[A, B, C](fab: Tuple2P[F, G, A, B]): Tuple2P[F, G, Either[A, C], Either[B, C]] =
        Tuple2P(fab._1.left, fab._2.left)

      override def right[A, B, C](fab: Tuple2P[F, G, A, B]): Tuple2P[F, G, Either[C, A], Either[C, B]] =
        Tuple2P(fab._1.right, fab._2.right)

      override def choice[A, B, C](f: Tuple2P[F, G, A, C], g: Tuple2P[F, G, B, C]): Tuple2P[F, G, Either[A, B], C] =
        Tuple2P(f._1 ||| g._1, f._2 ||| g._2)

      override def id[A]: Tuple2P[F, G, A, A] =
        Tuple2P(F.id, G.id)
    }
}

