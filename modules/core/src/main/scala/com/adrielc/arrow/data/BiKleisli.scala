package com.adrielc.arrow.data

import cats.arrow.Arrow
import cats.{Applicative, CoflatMap, Comonad, Distributive, FlatMap, Functor, Monad, MonoidK}
import com.adrielc.arrow.ArrowChoicePlus

case class BiKleisli[W[_], M[_], A, B](run: W[A] => M[B]) {

  def map[C](f: B => C)(implicit F: Functor[M]): BiKleisli[W, M, A, C] =
    BiKleisli(a => F.map(run(a))(f))

  def contramap[C](f: C => A)(implicit F: Functor[W]): BiKleisli[W, M, C, B] =
    BiKleisli(wc => run(F.map(wc)(f)))

  def andThen[C](g: BiKleisli[W, M, B, C])(implicit C: CoflatMap[W], F: FlatMap[M], D: Distributive[M]): BiKleisli[W, M, A, C] =
    BiKleisli(wa => F.flatMap(D.cosequence(C.map(C.coflatten(wa))(run)))(g.run))

  def choose[C, D](g: BiKleisli[W, M, C, D])(implicit A: MonoidK[M], M: Monad[M], D: Distributive[M], F: Functor[W]): BiKleisli[W, M, Either[A, C], Either[B, D]] =
    BiKleisli(weab => A.combineK(
      M.flatMap(D.distribute(weab)(_.fold(M.pure, _ => A.empty[A])))(m => M.map(run(m))(Left(_))),
      M.flatMap(D.distribute(weab)(_.fold(_ => A.empty[C], M.pure)))(m => M.map(g.run(m))(Right(_)))
    ))

  def first[C](implicit M: Applicative[M], C: Comonad[W]): BiKleisli[W, M, (A, C), (B, C)] =
    BiKleisli(wac => M.product(run(C.map(wac)(_._1)), M.pure(C.extract(wac)._2)))

  def plus(g: BiKleisli[W, M, A, B])(implicit M: MonoidK[M]): BiKleisli[W, M, A, B] =
    BiKleisli(wa => M.combineK(run(wa), g.run(wa)))
}

object BiKleisli {

  def lift[W[_], M[_], A, B](f: A => B)(implicit M: Monad[M], C: Comonad[W]): BiKleisli[W, M, A, B] =
    BiKleisli(wa => M.pure(f(C.extract(wa))))

  def empty[W[_], M[_], A, B](implicit M: MonoidK[M]): BiKleisli[W, M, A, B] =
    BiKleisli(_ => M.empty)

  implicit def arrowBiKleisli[W[_], M[_]](implicit C: Comonad[W], M: Monad[M], D: Distributive[M]): Arrow[BiKleisli[W, M, ?, ?]] =
    new Arrow[BiKleisli[W, M, ?, ?]] {

      def lift[A, B](f: A => B): BiKleisli[W, M, A, B] =
        BiKleisli.lift(f)

      def first[A, B, C](fa: BiKleisli[W, M, A, B]): BiKleisli[W, M, (A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: BiKleisli[W, M, B, C], g: BiKleisli[W, M, A, B]): BiKleisli[W, M, A, C] =
        g.andThen(f)
    }


  implicit def arrowChoicePlusBiKleisli[W[_], M[_]](implicit A: MonoidK[M], C: Comonad[W], M: Monad[M], D: Distributive[M]): ArrowChoicePlus[BiKleisli[W, M, ?, ?]] =
    new ArrowChoicePlus[BiKleisli[W, M, ?, ?]] {

      def zeroArrow[B, C]: BiKleisli[W, M, B, C] =
        BiKleisli.empty

      def plus[A, B](f: BiKleisli[W, M, A, B], g: BiKleisli[W, M, A, B]): BiKleisli[W, M, A, B] =
        f.plus(g)

      def choose[A, B, C, D](f: BiKleisli[W, M, A, C])(g: BiKleisli[W, M, B, D]): BiKleisli[W, M, Either[A, B], Either[C, D]] =
        f.choose(g)

      def lift[A, B](f: A => B): BiKleisli[W, M, A, B] =
        BiKleisli.lift(f)

      def first[A, B, C](fa: BiKleisli[W, M, A, B]): BiKleisli[W, M, (A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: BiKleisli[W, M, B, C], g: BiKleisli[W, M, A, B]): BiKleisli[W, M, A, C] =
        g.andThen(f)
    }
}
