package com.adrielc.quivr.data

import cats.{Applicative, Monoid}
import cats.arrow.{Arrow, ArrowChoice}
import cats.instances.int._

case class Scan[-A, +B](run: A => (Scan[A, B], B)) {

  def runList(data: List[A]): List[B] = Scan.runList(this, data)

  def first[C]: Scan[(A, C), (B, C)] = Scan { case (a, c) =>
    val (fa2, b) = run(a)
    (fa2.first, (b, c))
  }

  def andThen[C](f: Scan[B, C]): Scan[A, C] = Scan { a =>
    val (gg, b) = run(a)
    val (ff, c) = f.run(b)
    (gg.andThen(ff), c)
  }

  def lmap[Z](f: Z => A): Scan[Z, B] = Scan { z =>
    val (a, b) = run(f(z))
    (a.lmap(f), b)
  }

  def rmap[C](f: B => C): Scan[A, C] = Scan { a =>
    val (ab, b) = run(a)
    (ab.rmap(f), f(b))
  }
}

object Scan {

  def id[A]: Scan[A, A] = Scan(id -> _)

  def lift[A, B](f: A => B): Scan[A, B] = Scan(lift(f) -> f(_))

  def accum[A, B](b: B)(f: (A, B) => B): Scan[A, B] = Scan { a =>
    val b2 = f(a, b)
    (accum(b2)(f), b2)
  }

  def sum[A: Monoid]: Scan[A, A] = accum(Monoid.empty[A])(Monoid.combine[A])

  val count: Scan[Any, Int] = lift((_: Any) => 1).andThen(sum)

  implicit class ListOps[A](private val listA: List[A]) extends AnyVal {

    def scan[B](s: Scan[A, B]): List[B] = runList(s, listA)
  }

  implicit val arrowScan: Arrow[Scan] = new ArrowChoice[Scan] {

    override def lift[A, B](f: A => B): Scan[A, B] =
      Scan.lift(f)

    override def compose[A, B, C](f: Scan[B, C], g: Scan[A, B]): Scan[A, C] =
      g andThen f

    override def first[A, B, C](fa: Scan[A, B]): Scan[(A, C), (B, C)] =
      fa.first[C]

    override def choose[A, B, C, D](f: Scan[A, C])(g: Scan[B, D]): Scan[Either[A, B], Either[C, D]] = {
      import cats.implicits._
      Scan {
        case Left(value) =>
          val (a, b) = f.run(value)
          (choose(a)(g), b.asLeft)
        case Right(value) =>
          val (a, b) = g.run(value)
          (choose(f)(a), b.asRight)
      }
    }
  }

  private def runList[A, B](ff: Scan[A, B], as: List[A]): List[B] = as match {
    case h :: t =>
      val (ff2, b) = ff.run(h)
      b :: runList(ff2, t)
    case _ => List()
  }

  implicit def applicativeScan[I]: Applicative[Scan[I, *]] = new Applicative[Scan[I, *]] {

    override def pure[A](x: A): Scan[I, A] = ???

    override def ap[A, B](ff: Scan[I, A => B])(fa: Scan[I, A]): Scan[I, B] = Scan { i =>
      val (a, b) = ff.run(i)
      val (c, d) = fa.run(i)
      (ap(a)(c), b(d))
    }

    override def map[A, B](fa: Scan[I, A])(f: A => B): Scan[I, B] = fa.rmap(f)
  }
}