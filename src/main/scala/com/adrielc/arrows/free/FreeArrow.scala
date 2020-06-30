package com.adrielc.arrows
package free

import cats.arrow.Arrow
import cats.data.{AndThen => Then}
import cats.implicits._
import cats.{Applicative, Eval, Monoid}

sealed abstract class FreeArrow[+F[_, _], A, B]
  extends FreeArrowLike[FreeArrow, F, A, B] {

  /** Interprets/Runs the sequence of operations using the semantics of `Arrow[G]` */
  def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, B]

  def analyze[M: Monoid](fm: F ~~> λ[(α, β) => M]): M =
    foldMap(new (F ~~> ConstArr[M, ?, ?]) {
      def apply[C, D](f: F[C, D]): ConstArr[M, C, D] = ConstArr(fm(f))
    }).getConst


  def compile[G[_, _]](fg: F ~~> G): FreeArrow[G, A, B] =
    foldMap(new (F ~~> FreeArrow[G, ?, ?]) {
      def apply[C, D](f: F[C, D]): FreeArrow[G, C, D] = FreeArrow.lift(fg(f))
    })
}


object FreeArrow { self =>
  type FA[F[_, _], A, B] = FreeArrow[F, A, B]

  def unit: FA[Nothing, Unit, Unit] = id

  def id[A]: FA[Nothing, A, A] = Id()

  def pure[A, B](f: A => B): FA[Nothing, A, B] = Pure(Then(f))

  def lift[F[_, _], A, B](fab: F[A, B]): FA[F, A, B] = Lift(fab)

  def andThen[F[a, b], A, B, C](first: FA[F, A, B], next: FA[F, B, C]): FA[F, A, C] = (first, next) match {
    case (Pure(f), Pure(g)) => Pure(f andThen g)
    case (Pure(f), AndThen(Pure(g), e)) => AndThen(Pure(f andThen g), e)
    case (AndThen(a, Pure(f)), Pure(g)) => AndThen(a, Pure(f andThen g))
    case (AndThen(a, Pure(f)), AndThen(Pure(g), e)) => AndThen(a, AndThen(Pure(f andThen g), e))
    case _ => AndThen(first, next)
  }

  def rmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = fab match {
    case Pure(g) => Pure(g andThen f)
    case AndThen(a, Pure(g)) => AndThen(a, Pure(g andThen f))
    case _ => AndThen(fab, pure(f))
  }

  def lmap[F[_, _], A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = fab match {
    case Pure(g) => Pure(g compose f)
    case AndThen(Pure(g), a) => AndThen(Pure(g compose f), a)
    case _ => AndThen(pure(f), fab)
  }

  def first[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (A, C), (B, C)] = First(fab)

  def second[F[_, _], A, B, C](fab: FA[F, A, B]): FA[F, (C, A), (C, B)] = Second(fab)

  def split[F[_, _], A, B, C, D](fab: FA[F, A, B], fcd: FA[F, C, D]): FA[F, (A, C), (B, D)] = Split(fab , fcd)

  def merge[F[_, _], A, B, C](fab: FA[F, A, B], fac: FA[F, A, C]): FA[F, A, (B, C)] = (fab, fac) match {
    case (Pure(f), Pure(g)) => Pure(Then(a => (f(a), g(a))))
    case _ => Merge(fab, fac)
  }

  final private case class Id[A]() extends FA[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, A] = A.id
  }
  final private case class Pure[A, B](f: A => B) extends FA[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: Arrow[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FA[F, A, B], end: FA[F, B, C]) extends FA[F, A, C] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, C] = {
      val la = lazyAnd(fk)
      val eval = for {
        e <- la(begin)
        b <- Eval.later(la(end)).flatten
      } yield e andThen b
      eval.value
    }

    type EvalG[G[_, _], X, Y] = Eval[G[X, Y]]
    def lazyAnd[H[_, _], G[_, _]: Arrow](fk: H ~~> G): FA[H, ?, ?] ~~> EvalG[G, ?, ?] = new (FA[H, ?, ?] ~~> EvalG[G, ?, ?]) {
      def apply[D, E](f: FA[H, D, E]): EvalG[G, D, E] = f match {
        case a: AndThen[H, D, b, E] =>

          for {
            b <- Eval.later(apply(a.begin)).flatten
            e <- apply(a.end)
          } yield b.andThen(e)

        case _ => Eval.now(f.foldMap(fk))
      }
    }
  }
  final private case class Lift[F[_, _], A, B](fab: F[A, B]) extends FA[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, B] = fk(fab)
  }
  final private case class Merge[F[_, _], A, B, C](_first: FA[F, A, B], _second: FA[F, A, C]) extends FA[F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[A, (B, C)] = A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class Split[F[_, _], A, B, C, D](_first: FA[F, A, B], _second: FA[F, C, D]) extends FA[F, (A, C), (B, D)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(A, C), (B, D)] = A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private case class First[F[_, _], A, B, C](_first: FA[F, A, B]) extends FA[F, (A, C), (B, C)] {

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(A, C), (B, C)] = A.first(_first.foldMap(fk))
  }
  final private case class Second[F[_, _], A, B, C](_second: FA[F, A, B]) extends FA[F, (C, A), (C, B)] {

    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: Arrow[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }


  final implicit def freeArrArrow[F[_, _]]: Arrow[FA[F, ?, ?]] = new Arrow[FA[F, ?, ?]] {
    def lift[A, B](f: A => B): FA[F, A, B] = self.pure(f)
    def compose[A, B, C](f: FA[F, B, C], g: FA[F, A, B]): FA[F, A, C] = self.andThen(g, f)
    def first[A, B, C](fa: FA[F, A, B]): FA[F, (A, C), (B, C)] = self.first(fa)
    override def second[A, B, C](fa: FA[F, A, B]): FA[F, (C, A), (C, B)] = self.second(fa)
    override def rmap[A, B, C](fab: FA[F, A, B])(f: B => C): FA[F, A, C] = self.rmap(fab)(f)
    override def lmap[A, B, C](fab: FA[F, A, B])(f: C => A): FA[F, C, B] = self.lmap(fab)(f)
    override def split[A, B, C, D](f: FA[F, A, B], g: FA[F, C, D]): FA[F, (A, C), (B, D)] = self.split(f, g)
    override def merge[A, B, C](f: FA[F, A, B], g: FA[F, A, C]): FA[F, A, (B, C)] = self.merge(f, g)
  }

  implicit def freeArrowApplicative[F[_, _], C]: Applicative[FA[F, C, ?]] = new Applicative[FA[F, C, ?]] {
    def pure[A](x: A): FA[F, C, A] = FreeArrow.pure(_ => x)
    def ap[A, B](ff: FA[F, C, A => B])(fa: FA[F, C, A]): FA[F, C, B] = (ff &&& fa).rmap { case (f, a) => f(a) }
  }

//  implicit def toRefTree[F[_, _] : ArrToRefTree, A, B]: ToRefTree[FA[F, A, B]] = {
//
//    def toRef(fg: FA[F, _, _]): RefTree = fg match {
//
//      case Lift(arr) => arr.refTree
//
//      case Id() => idRefTree
//      case Pure(_) => pureRefTree
//      case m@Merge(l, r) => Ref(m.asInstanceOf[AnyRef], Seq(toRef(l).toField.withName("First"), toRef(r).toField.withName("Second")))
//      case s@Split(l, r) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(l).toField.withName("First"), toRef(r).toField.withName("Second")))
//      case s@AndThen(a, b) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(b).toField.withName("Begin"), toRef(a).toField.withName("End")))
//      case s@Second(f) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(f).toField))
//      case f@First(s) => Ref(f.asInstanceOf[AnyRef], Seq(toRef(s).toField))
//    }
//
//    ToRefTree(toRef)
//  }

}