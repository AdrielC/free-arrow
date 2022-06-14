package com.adrielc.quivr


import cats.data.{IRWST, Kleisli}
import cats.{Monad, Monoid, MonoidK, SemigroupK}
import cats.implicits._


object instances {

  object all extends AllInstances

  trait AllInstances extends Instances0 {

    implicit def kleisliArrowChoiceZero[M[_]](implicit M: Monad[M], MK: MonoidK[M]): ArrowChoiceZero[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoiceZero[M] {
        val monoidK: MonoidK[M] = MK
        val A: cats.arrow.ArrowChoice[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }

    implicit def arrowChoiceZeroForIRWST[E, L, O, F[_]]
    (implicit MK: MonoidK[F], ML: Monoid[L], M: Monad[F], MO: Monoid[O]): ArrowChoiceZero[IRWST[F, E, L, *, *, O]] =
      new catsIRWST.IRWST_ACZ[F, E, L, O] {
        val monad: Monad[F] = M
        val monoid: Monoid[L] = ML
        val monoidK: MonoidK[F] = MK
        val monoidO: Monoid[O] = MO
      }
  }

  trait Instances0 {

    implicit def arrowPlusForIRWST[E, L, O, F[_]]
    (implicit SK: SemigroupK[F], ML: Monoid[L], M: Monad[F], MO: Monoid[O]): ArrowChoicePlus[IRWST[F, E, L, *, *, O]] =
      new catsIRWST.IRWST_ACP[F, E, L, O] {
        val monad: Monad[F] = M
        val monoid: Monoid[L] = ML
        val semigroupK: SemigroupK[F] = SK
        val monoidO: Monoid[O] = MO
      }

    implicit def kleisliArrowChoicePlus[M[_]](implicit M: Monad[M], SK: SemigroupK[M]): ArrowChoicePlus[Kleisli[M, *, *]] =
      new kleisli.KleisliArrowChoicePlus[M] {
        val semigroupK: SemigroupK[M] = SK
        val A: cats.arrow.ArrowChoice[Kleisli[M, *, *]] = Kleisli.catsDataArrowChoiceForKleisli
      }
  }


  object kleisli {

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

    private[quivr] trait IRWST_ACZ[F[_], E, L, O] extends ArrowChoiceZero[IRWST[F, E, L, *, *, O]] with IRWST_ACP[F, E, L, O]  {

      implicit def monoidK: MonoidK[F]
      override def semigroupK: SemigroupK[F] = monoidK

      override def zeroArrow[B, C]: IRWST[F, E, L, B, C, O] =
        IRWST.apply((_: E, _: B) => monoidK.empty[(L, C, O)])
    }

    private[quivr] trait IRWST_ACP[F[_], E, L, O] extends ArrowChoicePlus[IRWST[F, E, L, *, *, O]] with IRWSTArrow[F, E, L, O]  {

      implicit def semigroupK: SemigroupK[F]

      override def choose[A, B, C, D](f: IRWST[F, E, L, A, C, O])(g: IRWST[F, E, L, B, D, O]): IRWST[F, E, L, Either[A, B], Either[C, D], O] =
        IRWST.apply((e: E, sa: Either[A, B]) => sa.fold(
          a => f.modify(_.asLeft[D]).run(e, a),
          b => g.modify(_.asRight[C]).run(e, b)
        ))

      override def plus[A, B](f: IRWST[F, E, L, A, B, O], g: IRWST[F, E, L, A, B, O]): IRWST[F, E, L, A, B, O] =
        IRWST.apply((e: E, a: A) => f.run(e, a) <+> g.run(e, a))
    }

    private[quivr] trait IRWSTArrow[F[_], E, L, O] extends Arrow[IRWST[F, E, L, *, *, O]] {
      import cats.data.IndexedReaderWriterStateT.catsDataStrongForIRWST

      implicit def monad: Monad[F]
      implicit def monoid: Monoid[L]
      implicit def monoidO: Monoid[O]

      override def split[A, B, C, D](fab: IRWST[F, E, L, A, B, O], fcd: IRWST[F, E, L, C, D, O]): IRWST[F, E, L, (A, C), (B, D), O] =
        IRWST.applyF {
          (fab.runF, fcd.runF).mapN { (a, c) =>
            (e: E, ac: (A, C)) => {
              val bout = a(e, ac._1)
              val dout = c(e, ac._2)
              (bout, dout).mapN { case ((l1, b, o1), (l2, d, o2)) =>  (l1 |+| l2, (b, d), o1 |+| o2) }
            }
          }
        }

      override def lift[A, B](f: A => B): IRWST[F, E, L, A, B, O] =
        IRWST.modify(f)(monad, monoid).map(_ => monoidO.empty)

      override def compose[A, B, C](f: IRWST[F, E, L, B, C, O], g: IRWST[F, E, L, A, B, O]): IRWST[F, E, L, A, C, O] =
        g.flatMap(o => f.map(o |+| _))

      override def first[A, B, C](fa: IRWST[F, E, L, A, B, O]): IRWST[F, E, L, (A, C), (B, C), O] =
        catsDataStrongForIRWST[F, E, L, O].first(fa)

      override def second[A, B, C](fa: IRWST[F, E, L, A, B, O]): IRWST[F, E, L, (C, A), (C, B), O] =
        catsDataStrongForIRWST[F, E, L, O].second(fa)

      override def lmap[A, B, C](fab: IRWST[F, E, L, A, B, O])(f: C => A): IRWST[F, E, L, C, B, O] =
        fab.contramap(f)

      override def rmap[A, B, C](fab: IRWST[F, E, L, A, B, O])(f: B => C): IRWST[F, E, L, A, C, O] =
        fab.modify(f)
    }
  }


  private[quivr] trait ComposedArrowInstance[F[_, _]] extends Arrow[F] {

    def A: cats.arrow.Arrow[F]

    def lift[A, B](f: A => B): F[A, B] = A.lift(f)
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] = A.compose(f, g)
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] = A.first(fa)
    override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = A.second(fa)
    override def id[A]: F[A, A] = A.id
    override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = A.split(f, g)
    override def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = A.merge(f, g)
    override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = A.rmap(fab)(f)
    override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] = A.lmap(fab)(f)
  }

  private[quivr] trait ComposedArrowChoiceInstance[~>[_, _]] extends ComposedArrowInstance[~>] with ArrowChoice[~>] {

    def A: cats.arrow.ArrowChoice[~>]

    def choose[A, B, C, D](f: A ~> C)(g: B ~> D): Either[A, B] ~> Either[C, D] = A.choose(f)(g)
    override def choice[A, B, C](f: A ~> C, g: B ~> C): Either[A, B] ~> C = A.choice(f, g)
    override def left[A, B, C](fab: A ~> B): Either[A, C] ~> Either[B, C] = A.left(fab)
    override def right[A, B, C](fab: A ~> B): Either[C, A] ~> Either[C, B] = A.right(fab)
  }
}
