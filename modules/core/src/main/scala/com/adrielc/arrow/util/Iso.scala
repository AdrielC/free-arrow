package com.adrielc.arrow.util

import cats.arrow.{Arrow, Compose}
import cats.instances.function._
import cats.syntax.{compose, either}, either._, compose._

trait Iso[F[_, _], A, B] { self =>

  implicit def C: Compose[F]

  def to: F[A, B]

  def from: F[B, A]

  def andThen[C](other: Iso[F, B, C]): Iso[F, A, C] =
    Iso(to >>> other.to, from <<< other.from)

  def compose[C](other: Iso[F, C, A]): Iso[F, C, B] =
    other.andThen(self)
}

object Iso {


  def apply[F[_, _]: Compose, A, B](
    _to   : F[A, B],
    _from : F[B, A]
  ): Iso[F, A, B] = new Iso[F, A, B] {
    val C: Compose[F] = Compose[F]
    val to: F[A, B] = _to
    val from: F[B, A] = _from
  }

  def id[F[_, _], A](implicit A: Arrow[F]): Iso[F, A, A] = Iso(A.id[A], A.id[A])

  type NIso[A, B] = Iso[Function1, A, B]
  type <=>[A, B] = NIso[A, B]

  object NIso {

    def apply[A, B](to: A => B, from: B => A): A <=> B =
      Iso(to, from)

    def id[A]: A <=> A =
      Iso.id[Function1, A]
  }

  implicit val nisoDistributesOverProduct: Distributes[NIso, Tuple2] =
    new Distributes[NIso, Tuple2] {

      def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[(A0, B0), (A1, B1)] =
        NIso(p0 => (pa.to(p0._1), pb.to(p0._2)), p1 => (pa.from(p1._1), pb.from(p1._2)))
    }

  implicit val nisoDistributesOverSum: Distributes[NIso, Either] = new Distributes[NIso, Either] {

    def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[Either[A0, B0], Either[A1, B1]] =
      NIso(e0 => e0.bimap(pa.to, pb.to), e1 => e1.bimap(pa.from, pb.from))
  }
}