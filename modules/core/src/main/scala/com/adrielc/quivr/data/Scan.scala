package com.adrielc.quivr.data

import cats.Monoid
import cats.arrow.Arrow
import cats.instances.int._

case class Scan[-A, B](run: A => (Scan[A, B], B)) {

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

  implicit val arrowScan: Arrow[Scan] = new Arrow[Scan] {

    override def lift[A, B](f: A => B): Scan[A, B] =
      Scan.lift(f)

    override def compose[A, B, C](f: Scan[B, C], g: Scan[A, B]): Scan[A, C] =
      g andThen f

    override def first[A, B, C](fa: Scan[A, B]): Scan[(A, C), (B, C)] =
      fa.first[C]
  }

  private def runList[A, B](ff: Scan[A, B], as: List[A]): List[B] = as match {
    case h :: t =>
      val (ff2, b) = ff.run(h)
      b :: runList(ff2, t)
    case _ => List()
  }
}