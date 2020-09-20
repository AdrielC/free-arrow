package com.adrielc.quivr.data

import cats.arrow.ArrowChoice
import cats.syntax.arrowChoice._
import cats.syntax.choice._
import cats.syntax.compose._
import cats.syntax.strong._


case class BiTuple2K[+F[_, _], +G[_, _], A, B](_1: F[A, B], _2: G[A, B]) {

  def swap: BiTuple2K[G, F, A, B] = BiTuple2K(_2, _1)
}

object BiTuple2K {

  implicit def tuple2HKArrow[F[_, _], G[_, _]]
  (implicit F: ArrowChoice[F], G: ArrowChoice[G]): ArrowChoice[BiTuple2K[F, G, *, *]] =
    new ArrowChoice[BiTuple2K[F, G, *, *]] {

      def choose[A, B, C, D](f: BiTuple2K[F, G, A, C])(g: BiTuple2K[F, G, B, D]): BiTuple2K[F, G, Either[A, B], Either[C, D]] =
        BiTuple2K(f._1 +++ g._1, f._2 +++ g._2)

      def lift[A, B](f: A => B): BiTuple2K[F, G, A, B] =
        BiTuple2K(F.lift(f), G.lift(f))

      def compose[A, B, C](f: BiTuple2K[F, G, B, C], g: BiTuple2K[F, G, A, B]): BiTuple2K[F, G, A, C] =
        BiTuple2K(g._1 >>> f._1, g._2 >>> f._2)

      def first[A, B, C](fa: BiTuple2K[F, G, A, B]): BiTuple2K[F, G, (A, C), (B, C)] =
        BiTuple2K(fa._1.first, fa._2.first)

      override def second[A, B, C](fa: BiTuple2K[F, G, A, B]): BiTuple2K[F, G, (C, A), (C, B)] =
        BiTuple2K(fa._1.second, fa._2.second)

      override def left[A, B, C](fab: BiTuple2K[F, G, A, B]): BiTuple2K[F, G, Either[A, C], Either[B, C]] =
        BiTuple2K(fab._1.left, fab._2.left)

      override def right[A, B, C](fab: BiTuple2K[F, G, A, B]): BiTuple2K[F, G, Either[C, A], Either[C, B]] =
        BiTuple2K(fab._1.right, fab._2.right)

      override def choice[A, B, C](f: BiTuple2K[F, G, A, C], g: BiTuple2K[F, G, B, C]): BiTuple2K[F, G, Either[A, B], C] =
        BiTuple2K(f._1 ||| g._1, f._2 ||| g._2)

      override def id[A]: BiTuple2K[F, G, A, A] =
        BiTuple2K(F.id, G.id)
    }
}

