package com.adrielc.arrow.data

import cats.arrow.ArrowChoice
import cats.syntax.arrowChoice._
import cats.syntax.choice._
import cats.syntax.compose._
import cats.syntax.strong._


case class Tuple2A[F[_, _], G[_, _], A, B](_1: F[A, B], _2: G[A, B])

object Tuple2A {

  implicit def tuple2HKArrow[F[_, _], G[_, _]]
  (implicit F: ArrowChoice[F], G: ArrowChoice[G]): ArrowChoice[Tuple2A[F, G, ?, ?]] =
    new ArrowChoice[Tuple2A[F, G, ?, ?]] {

      def choose[A, B, C, D](f: Tuple2A[F, G, A, C])(g: Tuple2A[F, G, B, D]): Tuple2A[F, G, Either[A, B], Either[C, D]] =
        Tuple2A(f._1 +++ g._1, f._2 +++ g._2)

      def lift[A, B](f: A => B): Tuple2A[F, G, A, B] =
        Tuple2A(F.lift(f), G.lift(f))

      def compose[A, B, C](f: Tuple2A[F, G, B, C], g: Tuple2A[F, G, A, B]): Tuple2A[F, G, A, C] =
        Tuple2A(g._1 >>> f._1, g._2 >>> f._2)

      def first[A, B, C](fa: Tuple2A[F, G, A, B]): Tuple2A[F, G, (A, C), (B, C)] =
        Tuple2A(fa._1.first, fa._2.first)

      override def second[A, B, C](fa: Tuple2A[F, G, A, B]): Tuple2A[F, G, (C, A), (C, B)] =
        Tuple2A(fa._1.second, fa._2.second)

      override def left[A, B, C](fab: Tuple2A[F, G, A, B]): Tuple2A[F, G, Either[A, C], Either[B, C]] =
        Tuple2A(fab._1.left, fab._2.left)

      override def right[A, B, C](fab: Tuple2A[F, G, A, B]): Tuple2A[F, G, Either[C, A], Either[C, B]] =
        Tuple2A(fab._1.right, fab._2.right)

      override def choice[A, B, C](f: Tuple2A[F, G, A, C], g: Tuple2A[F, G, B, C]): Tuple2A[F, G, Either[A, B], C] =
        Tuple2A(f._1 ||| g._1, f._2 ||| g._2)

      override def id[A]: Tuple2A[F, G, A, A] =
        Tuple2A(F.id, G.id)
    }
}

