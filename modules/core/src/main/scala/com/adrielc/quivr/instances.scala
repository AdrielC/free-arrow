package com.adrielc.quivr


import cats.data.{IRWST, Kleisli}
import cats.{Monad, Monoid, MonoidK, Parallel, SemigroupK, Traverse}
import cats.implicits._
import com.adrielc.quivr.instances.catsIRWST.IRWSTArrowPar

object instances {

  object all extends Instances0 {

    implicit def kleisliArrowChoicePlus[M[_]](implicit M: Monad[M], SK: SemigroupK[M]): ArrowChoicePlus[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoicePlus[M] {
        val semigroupK: SemigroupK[M] = SK
        val A: AC[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }

    implicit def kleisliTraverseParArrowChoicePlus[M[_]](implicit P: Parallel[M], SK: SemigroupK[M], T: Traverse[M])
    : ArrowChoicePlus[Kleisli[M, *, *]] with ArrowParallel[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoicePlus[M] with kleisli.KleisliTraverseArrowChoice[M] {
        val traverse: Traverse[M] = T
        val semigroupK: SemigroupK[M] = SK
        val parallel: Parallel[M] = P
        val A: AC[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }

    implicit def arrowPlusForIRWST[E, L, F[_]](implicit SK: SemigroupK[F], ML: Monoid[L], P: Parallel[F])
    : ArrowChoicePlus[IRWST[F, E, L, *, *, Unit]]
      with ArrowParallel[IRWST[F, E, L, *, *, Unit]] =
      new ArrowChoicePlus[IRWST[F, E, L, *, *, Unit]] with IRWSTArrowPar[F, E, L]  {
        val parallel: Parallel[F] = P
        val monoid: Monoid[L] = ML

        override def choose[A, B, C, D](f: IRWST[F, E, L, A, C, Unit])(g: IRWST[F, E, L, B, D, Unit]): IRWST[F, E, L, Either[A, B], Either[C, D], Unit] =
          IRWST.apply((e: E, sa: Either[A, B]) => sa.fold(
            a => f.modify(_.asLeft[D]).run(e, a),
            b => g.modify(_.asRight[C]).run(e, b)
          ))

        override def plus[A, B](f: IRWST[F, E, L, A, B, Unit], g: IRWST[F, E, L, A, B, Unit]): IRWST[F, E, L, A, B, Unit] =
          IRWST.apply((e: E, a: A) => f.run(e, a) <+> g.run(e, a))
      }
  }

  trait Instances0 extends Instances1 {

    implicit def kleisliParArrowChoicePlus[M[_]](implicit P: Parallel[M], SK: SemigroupK[M])
    : ArrowChoicePlus[Kleisli[M, *, *]] with ArrowParallel[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoicePlus[M] with kleisli.ParKleisliArrowChoice[M] {
        val semigroupK: SemigroupK[M] = SK
        val parallel: Parallel[M] = P
        val A: AC[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }
  }

  trait Instances1 {

    implicit def kleisliArrowChoiceZero[M[_]](implicit M: Monad[M], MK: MonoidK[M]): ArrowChoiceZero[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoiceZero[M] {
        val monoidK: MonoidK[M] = MK
        val A: AC[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }
  }





  object kleisli {

    private[quivr] trait KleisliTraverseArrowChoice[F[_]] extends ParKleisliArrowChoice[F]
      with ComposedArrowInstance[Kleisli[F, *, *]] {
      implicit def traverse: Traverse[F]

      override def andThen[A, B, C](f: Kleisli[F, A, B], g: Kleisli[F, B, C]): Kleisli[F, A, C] =
        Kleisli { a => f.run(a).parTraverse(g.run).flatten }
    }


    private[quivr] trait ParKleisliArrowChoice[F[_]] extends ArrowParallel[Kleisli[F, *, *]]
      with ComposedArrowInstance[Kleisli[F, *, *]] {
      implicit def parallel: Parallel[F]
      implicit def monad: Monad[F] = parallel.monad

      override def splitPar[A, B, C, D](fab: Kleisli[F, A, B], fcd: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
        Kleisli { case (a, c) => (fab.run(a), fcd.run(c)).parTupled }
    }

    private[quivr] trait KleisliArrowChoicePlus[M[_]] extends ArrowChoicePlus[Kleisli[M, *, *]] with ComposedArrowChoiceInstance[Kleisli[M, *, *]] {
      implicit def semigroupK: SemigroupK[M]

      def plus[A, B](f: Kleisli[M, A, B], g: Kleisli[M, A, B]): Kleisli[M, A, B] =
        Kleisli[M, A, B](a => semigroupK.combineK(f.run(a), g.run(a)))
    }

    private[quivr] trait KleisliArrowChoiceZero[M[_]] extends KleisliArrowChoicePlus[M] with ArrowChoiceZero[Kleisli[M, *, *]] {
      def monoidK: MonoidK[M]
      def semigroupK: SemigroupK[M] = monoidK
      override def zeroArrow[B, C]: Kleisli[M, B, C] =
        Kleisli(_ => monoidK.empty)
    }
  }



  object catsIRWST {

    private[quivr] trait IRWSTArrowPar[F[_], E, L] extends ArrowParallel[IRWST[F, E, L, *, *, Unit]] {
      import cats.data.IndexedReaderWriterStateT.catsDataStrongForIRWST

      implicit def parallel: Parallel[F]
      implicit def monad: Monad[F] = parallel.monad
      implicit def monoid: Monoid[L]

      override def splitPar[A, B, C, D](fab: IRWST[F, E, L, A, B, Unit], fcd: IRWST[F, E, L, C, D, Unit]): IRWST[F, E, L, (A, C), (B, D), Unit] =
        IRWST.applyF {
          (fab.runF, fcd.runF).parMapN { (a, c) =>
            (e: E, ac: (A, C)) => {
              val bout = a(e, ac._1)
              val dout = c(e, ac._2)
              (bout, dout).parMapN { case ((l1, b, _), (l2, d, _)) =>  (l1 |+| l2, (b, d), ()) }
            }
          }
        }

      override def lift[A, B](f: A => B): IRWST[F, E, L, A, B, Unit] =
        IRWST.modify(f)

      override def compose[A, B, C](f: IRWST[F, E, L, B, C, Unit], g: IRWST[F, E, L, A, B, Unit]): IRWST[F, E, L, A, C, Unit] =
        g.flatMap(_ => f)

      override def split[A, B, C, D](f: IRWST[F, E, L, A, B, Unit], g: IRWST[F, E, L, C, D, Unit]): IRWST[F, E, L, (A, C), (B, D), Unit] =
        splitPar(f, g)

      override def first[A, B, C](fa: IRWST[F, E, L, A, B, Unit]): IRWST[F, E, L, (A, C), (B, C), Unit] =
        catsDataStrongForIRWST[F, E, L, Unit].first(fa)

      override def second[A, B, C](fa: IRWST[F, E, L, A, B, Unit]): IRWST[F, E, L, (C, A), (C, B), Unit] =
        catsDataStrongForIRWST[F, E, L, Unit].second(fa)

      override def lmap[A, B, C](fab: IRWST[F, E, L, A, B, Unit])(f: C => A): IRWST[F, E, L, C, B, Unit] =
        fab.contramap(f)

      override def rmap[A, B, C](fab: IRWST[F, E, L, A, B, Unit])(f: B => C): IRWST[F, E, L, A, C, Unit] =
        fab.modify(f)
    }
  }
}
