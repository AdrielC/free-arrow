package com.adrielc.quivr.data

import cats.Bifunctor
import cats.arrow.{Category, Compose}
import cats.implicits._
import com.adrielc.quivr.BiDistributes

case class Iso[F[_, _], A, B](to: F[A, B], from: F[B, A]) { self =>

  def andThen[C](other: Iso[F, B, C])(implicit C: Compose[F]): Iso[F, A, C] =
    Iso(to >>> other.to, from <<< other.from)

  def compose[C](other: Iso[F, C, A])(implicit C: Compose[F]): Iso[F, C, B] =
    other.andThen(self)
}

object Iso {

  def id[F[_, _], A](implicit C: Category[F]): Iso[F, A, A] = Iso(C.id[A], C.id[A])

  type NIso[A, B] = Iso[Function1, A, B]
  type <=>[A, B] = NIso[A, B]

  object NIso {

    def apply[A, B](to: A => B, from: B => A): A <=> B =
      Iso(to, from)

    def id[A]: A <=> A =
      Iso.id[Function1, A]
  }

  implicit def nisoDistributesOberBifunctor[F[_, _]: Bifunctor]: BiDistributes[NIso, F] =
    new BiDistributes[NIso, F] {

      def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[F[A0, B0], F[A1, B1]] =
        NIso(p0 => p0.bimap(pa.to, pb.to), p1 => p1.bimap(pa.from, pb.from))
    }

  implicit val nisoDistributesOverMap: BiDistributes[NIso, Map] = new BiDistributes[NIso, Map] {

    override def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[Map[A0, B0], Map[A1, B1]] =
      NIso(e0 => e0.map { case (k, v) => pa.to(k) -> pb.to(v) }, e1 => e1.map { case (k, v) => pa.from(k) -> pb.from(v) })
  }
}