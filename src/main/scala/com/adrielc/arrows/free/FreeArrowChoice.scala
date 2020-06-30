package com.adrielc.arrows
package free

import cats.arrow.ArrowChoice
import cats.data.{AndThen => Then}
import cats.implicits._
import cats.{Eval, Monoid}
import com.adrielc.arrows.free.FreeArrowLike.FreeArrowChoiceLike

sealed abstract class FreeArrowChoice[+F[_, _], A, B]
  extends FreeArrowChoiceLike[FreeArrowChoice, F, A, B] {

  /** Interprets/Runs the sequence of operations using the semantics of `Arrow[G]` */
  def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, B]

  def analyze[M: Monoid](fm: F ~~> λ[(α, β) => M]): M =
    foldMap(new (F ~~> ConstArr[M, ?, ?]) {
      def apply[C, D](f: F[C, D]): ConstArr[M, C, D] = ConstArr(fm(f))
    }).getConst

  def compile[G[_, _]](fg: F ~~> G): FreeArrowChoice[G, A, B] =
    foldMap(new (F ~~> FreeArrowChoice[G, ?, ?]) {
      def apply[C, D](f: F[C, D]): FreeArrowChoice[G, C, D] = FreeArrowChoice.lift(fg(f))
    })
}

object FreeArrowChoice { self =>
  type FAC[F[_, _], A, B] = FreeArrowChoice[F, A, B]

  def id[A]: FAC[Nothing, A, A] = Id()

  def pure[A, B](f: A => B): FAC[Nothing, A, B] = Pure(Then(f))

  def lift[F[_, _], A, B](fab: F[A, B]): FAC[F, A, B] = Lift(fab)

  def andThen[F[a, b], A, B, C](first: FAC[F, A, B], next: FAC[F, B, C]): FAC[F, A, C] = (first, next) match {
    case (Pure(f), Pure(g)) => Pure(f andThen g)
    case (Pure(f), AndThen(Pure(g), e)) => AndThen(Pure(f andThen g), e)
    case (AndThen(a, Pure(f)), Pure(g)) => AndThen(a, Pure(f andThen g))
    case (AndThen(a, Pure(f)), AndThen(Pure(g), e)) => AndThen(a, AndThen(Pure(f andThen g), e))
    case _ => AndThen(first, next)
  }

  def rmap[F[_, _], A, B, C](fab: FAC[F, A, B])(f: B => C): FAC[F, A, C] = fab match {
    case Pure(g) => Pure(g andThen f)
    case AndThen(a, Pure(g)) => AndThen(a, Pure(g andThen f))
    case _ => AndThen(fab, pure(f))
  }

  def lmap[F[_, _], A, B, C](fab: FAC[F, A, B])(f: C => A): FAC[F, C, B] = fab match {
    case Pure(g) => Pure(g compose f)
    case AndThen(Pure(g), a) => AndThen(Pure(g compose f), a)
    case _ => AndThen(pure(f), fab)
  }

  def merge[F[_, _], A, B, C](fab: FAC[F, A, B], fac: FAC[F, A, C]): FAC[F, A, (B, C)] = (fab, fac) match {
    case (Pure(f), Pure(g)) => Pure(Then(a => (f(a), g(a))))
    case _ => Merge(fab, fac)
  }

  def first[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, (A, C), (B, C)] = First(fab)
  def second[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, (C, A), (C, B)] = Second(fab)
  def split[F[_, _], A, B, C, D](fab: FAC[F, A, B], fcd: FAC[F, C, D]): FAC[F, (A, C), (B, D)] = Split(fab, fcd)
  def left[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, Either[A, C], Either[B, C]] = Left(fab)
  def right[F[_, _], A, B, C](fab: FAC[F, A, B]): FAC[F, Either[C, A], Either[C, B]] = Right(fab)
  def choose[F[_, _], A, B, C, D](fab: FAC[F, A, B], fcd: FAC[F, C, D]): FAC[F, Either[A, C], Either[B, D]] = Choose(fab, fcd)
  def choice[F[_, _], A, B, C](fab: FAC[F, A, B], fcd: FAC[F, C, B]): FAC[F, Either[A, C], B] = Choice(fab, fcd)

  final private[arrows] case class Id[A]() extends FAC[Nothing, A, A] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowChoice[G]): G[A, A] = A.id
  }
  final private[arrows] case class Pure[A, B](f: A => B) extends FAC[Nothing, A, B] {
    def foldMap[G[_, _]](fk: Nothing ~~> G)(implicit A: ArrowChoice[G]): G[A, B] = A.lift(f)
  }
  final private case class AndThen[F[_, _], A, B, C](begin: FAC[F, A, B], end: FAC[F, B, C]) extends FAC[F, A, C] {
    type EvalG[G[_, _], X, Y] = Eval[G[X, Y]]

    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, C] = {
      val la = lazyAnd(fk)
      val eval = for {
        e <- la(begin)
        b <- Eval.later(la(end)).flatten
      } yield e andThen b
      eval.value
    }

    def lazyAnd[H[_, _], G[_, _]: ArrowChoice](fk: H ~~> G): FAC[H, ?, ?] ~~> EvalG[G, ?, ?] =
      new (FAC[H, ?, ?] ~~> EvalG[G, ?, ?]) {
        def apply[D, E](f: FAC[H, D, E]): EvalG[G, D, E] = f match {
          case a: AndThen[H, D, b, E] =>

            for {
              b <- Eval.later(apply(a.begin)).flatten
              e <- apply(a.end)
            } yield b.andThen(e)

          case _ => Eval.now(f.foldMap(fk))
        }
      }
  }
  final private[arrows] case class Lift[F[_, _], A, B](fab: F[A, B]) extends FAC[F, A, B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, B] = fk(fab)
  }
  final private[arrows] case class Merge[F[_, _], A, B, C](_first: FAC[F, A, B], _second: FAC[F, A, C]) extends FAC[F, A, (B, C)] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[A, (B, C)] = A.merge(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private[arrows] case class Split[F[_, _], A, B, C, D](_first: FAC[F, A, B], _second: FAC[F, C, D]) extends FAC[F, (A, C), (B, D)] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(A, C), (B, D)] = A.split(_first.foldMap(fk), _second.foldMap(fk))
  }
  final private[arrows] case class First[F[_, _], A, B, C](_first: FAC[F, A, B]) extends FAC[F, (A, C), (B, C)] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(A, C), (B, C)] = A.first(_first.foldMap(fk))
  }
  final private[arrows] case class Second[F[_, _], A, B, C](_second: FAC[F, A, B]) extends FAC[F, (C, A), (C, B)] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[(C, A), (C, B)] = A.second(_second.foldMap(fk))
  }
  final private[arrows] case class Choose[F[_, _], A, B, C, D](_left: FAC[F, A, B], _right: FAC[F, C, D]) extends FAC[F, Either[A, C], Either[B, D]] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], Either[B, D]] = A.choose(_left.foldMap(fk))(_right.foldMap(fk))
  }
  final private[arrows] case class Left[F[_, _], A, B, C](_left: FAC[F, A, B]) extends FAC[F, Either[A, C], Either[B, C]] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], Either[B, C]] = A.left(_left.foldMap(fk))
  }
  final private[arrows] case class Right[F[_, _], A, B, C](_right: FAC[F, A, B]) extends FAC[F, Either[C, A], Either[C, B]] {
    override def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[C, A], Either[C, B]] = A.right(_right.foldMap(fk))
  }
  final private[arrows] case class Choice[F[_, _], A, B, C](_left: FAC[F, A, B], _right: FAC[F, C, B]) extends FAC[F, Either[A, C], B] {
    def foldMap[G[_, _]](fk: F ~~> G)(implicit A: ArrowChoice[G]): G[Either[A, C], B] = A.choice(_left.foldMap(fk), _right.foldMap(fk))
  }


  final implicit def freeArrArrowChoice[F[_, _]]: ArrowChoice[FAC[F, ?, ?]] = new ArrowChoice[FAC[F, ?, ?]] {
    def lift[A, B](f: A => B): FAC[F, A, B] = self.pure(f)
    def choose[A, B, C, D](f: FAC[F, A, C])(g: FAC[F, B, D]): FAC[F, Either[A, B], Either[C, D]] = self.choose(f, g)
    def first[A, B, C](fa: FAC[F, A, B]): FAC[F, (A, C), (B, C)] = self.first(fa)
    def compose[A, B, C](f: FAC[F, B, C], g: FAC[F, A, B]): FAC[F, A, C] = self.andThen(g, f)
    override def id[A]: FAC[F, A, A] = self.id
    override def rmap[A, B, C](fab: FAC[F, A, B])(f: B => C): FAC[F, A, C] = self.rmap(fab)(f)
    override def lmap[A, B, C](fab: FAC[F, A, B])(f: C => A): FAC[F, C, B] = self.lmap(fab)(f)
    override def second[A, B, C](fa: FAC[F, A, B]): FAC[F, (C, A), (C, B)] = self.second(fa)
    override def choice[A, B, C](f: FAC[F, A, C], g: FAC[F, B, C]): FAC[F, Either[A, B], C] = self.choice(f, g)
    override def split[A, B, C, D](f: FAC[F, A, B], g: FAC[F, C, D]): FAC[F, (A, C), (B, D)] = self.split(f, g)
    override def merge[A, B, C](f: FAC[F, A, B], g: FAC[F, A, C]): FAC[F, A, (B, C)] = self.merge(f, g)
    override def left[A, B, C](fab: FAC[F, A, B]): FAC[F, Either[A, C], Either[B, C]] = self.left(fab)
    override def right[A, B, C](fab: FAC[F, A, B]): FAC[F, Either[C, A], Either[C, B]] = self.right(fab)
  }

//
//  implicit def toRefTree[F[_, _] : ArrToRefTree, A, B]: ToRefTree[FAC[F, A, B]] = {
//
//    def toRef(fg: FAC[F, _, _]): RefTree = fg match {
//
//      case Lift(arr) => arr.refTree
//
//      case Pure(_) => pureRefTree
//      case Id() => idRefTree
//      case s@AndThen(a, b) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(a).toField.withName("Begin"), toRef(b).toField.withName("End")))
//      case m@Merge(l, r) => Ref(m.asInstanceOf[AnyRef], Seq(toRef(l).toField.withName("First"), toRef(r).toField.withName("Second")))
//      case s@Split(l, r) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(l).toField.withName("First"), toRef(r).toField.withName("Second")))
//      case c@Choose(a, b) => Ref(c.asInstanceOf[AnyRef], Seq(toRef(a).toField.withName("Left"), toRef(b).toField.withName("Right")))
//      case c@Choice(a, b) => Ref(c.asInstanceOf[AnyRef], Seq(toRef(a).toField.withName("Left"), toRef(b).toField.withName("Right")))
//      case s@Second(f) => Ref(s.asInstanceOf[AnyRef], Seq(toRef(f).toField))
//      case f@First(s) => Ref(f.asInstanceOf[AnyRef], Seq(toRef(s).toField))
//      case l@Left(f) => Ref(l.asInstanceOf[AnyRef], Seq(toRef(f).toField))
//      case r@Right(f) => Ref(r.asInstanceOf[AnyRef], Seq(toRef(f).toField))
//    }
//
//    ToRefTree(toRef)
//  }
}