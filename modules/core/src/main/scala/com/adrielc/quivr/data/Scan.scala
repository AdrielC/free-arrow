package com.adrielc.quivr.data

import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.{Foldable, Monoid, MonoidK}
import cats.arrow.{Arrow, ArrowChoice}
import cats.implicits._
import cats.free.Cofree
import com.twitter.algebird.DecayedValue

import scala.concurrent.duration.DurationInt

case class Scan[-A, B](run: A => (Scan[A, B], B)) {

  def runList(data: List[A]): List[B] = Scan.runList(this, data)

  def runStream(data: Stream[A]): Stream[B] = Scan.runStream(this, data)

  def fold[F[_]: Foldable, AA <: A: Monoid](data: F[AA])(implicit M: Monoid[B]): (Scan[AA, B], B) = Scan.fold(this, data)

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

  type ScanF[F[_, _], A, B] = Cofree[λ[b => F[A, b]], B]

  def scan[F[_, _]: Arrow, A, B](b: B)(f: (A, B) => B): ScanF[F, A, B] =
    Cofree(b, cats.Eval.later(Arrow[F].lift(a => scan[F, A, B](f(a, b))(f))))

  def id[A]: Scan[A, A] = Scan(id -> _)

  def lift[A, B](f: A => B): Scan[A, B] = Scan(lift(f) -> f(_))

  def accum[A, B](b: B)(f: (A, B) => B): Scan[A, B] = Scan { a =>
    val b2 = f(a, b)
    (accum(b2)(f), b2)
  }

  def accum[A, B](f: (A, B) => B): Scan[(A, B), B] = Scan { case (a, b) =>
    val b2 = f(a, b)
    (accum(f), b2)
  }

  def cumSum[A: Monoid]: Scan[A, A] = accum(Monoid.empty[A])(Monoid.combine[A])

  def cumCount[A]: Scan[A, Int] = cumSum[Int].lmap(_ => 1)

  implicit def scanMonoid[A, B: Monoid]: Monoid[Scan[A, B]] = new Monoid[Scan[A, B]] {
    override def empty: Scan[A, B] = Scan.lift(_ => Monoid.empty[B])
    override def combine(x: Scan[A, B], y: Scan[A, B]): Scan[A, B] = Scan { a =>
      val (xx, b1) = x.run(a)
      val (yy, b2) = y.run(a)
      (combine(xx, yy), b1 |+| b2)
    }
  }

  implicit def arrowMonoidK[F[_, _]: Arrow]: MonoidK[λ[a => F[a, a]]] = new MonoidK[λ[a => F[a, a]]] {
    override def empty[A]: F[A, A] = Arrow[F].id
    override def combineK[A](x: F[A, A], y: F[A, A]): F[A, A] = x >>> y
  }

  implicit val arrowScan: ArrowChoice[Scan] = new ArrowChoice[Scan] {

    override def lift[A, B](f: A => B): Scan[A, B] =
      Scan.lift(f)

    override def compose[A, B, C](f: Scan[B, C], g: Scan[A, B]): Scan[A, C] =
      g andThen f

    override def first[A, B, C](fa: Scan[A, B]): Scan[(A, C), (B, C)] =
      fa.first[C]

    override def choose[A, B, C, D](f: Scan[A, C])(g: Scan[B, D]): Scan[Either[A, B], Either[C, D]] = {
      Scan { _.fold(
          a => {
            val (s, c) = f.run(a)
            choose(s)(g) -> c.asLeft[D]
          },
          b => {
            val (s, d) = g.run(b)
            choose(f)(s) -> d.asRight[C]
          })
      }
    }
  }

  private def fold[F[_]: Foldable, A: Monoid, B: Monoid](ff: Scan[A, B], as: F[A]): (Scan[A, B], B) =
    as.foldLeft(ff.run(Monoid.empty[A])) { case ((f, b), a) =>
      val (ff, o) = f.run(a)
      (ff, b |+| o)
    }

  def runList[A, B](ff: Scan[A, B], as: List[A]): List[B] =
    as match {
      case h :: t =>
        val (ff2, b) = ff.run(h)
        b :: runList(ff2, t)
      case _ => List.empty
    }

  def runStream[A, B](ff: Scan[A, B], as: Stream[A]): Stream[B] =
    as match {
      case h #:: t =>
        val (ff2, b) = ff.run(h)
        b #:: runStream(ff2, t)
      case _ => Stream.empty
    }
}


object ScanTest extends App {

  import com.twitter.algebird.Moments
  val now = Instant.now().truncatedTo(ChronoUnit.MINUTES)

  DecayedValue.build(
    1,
    now.toEpochMilli.toDouble,
    30.minutes.toMillis.toDouble)

  val s = Scan.scan(0)((a: Int, b) => a + b).map(Moments(_))

  val cumsum = Scan.accum(0)((a: Int, b) => 1 + (a + b))

  val timestamp = Scan.accum[Int, (Int, Instant)]((1, now)) {
    case (a: Int, (i, t)) => (i + a) -> t.plus((10 * (a + i + 1).toDouble).toLong, ChronoUnit.MINUTES) }

  val decay = Scan.lift[(Int, Instant), (Moments, DecayedValue)] { case (v, t) => (
    Moments(v),
    DecayedValue.build(1, t.toEpochMilli.toDouble, 30.millis.toMillis.toDouble))
  }

  implicit val monoid = DecayedValue.monoidWithEpsilon(0.001)

  val exp = Scan.accum[(Moments, DecayedValue), (Moments, DecayedValue)](
    (Monoid.empty[Moments], monoid.zero)) { case (
    (m: Moments, decay: DecayedValue), (m2: Moments, decay2: DecayedValue)) =>
    (m.scale((decay |+| decay2).value) |+| m2, decay |+| decay2)
  }

  val decayedValues = cumsum >>> timestamp >>> decay >>> exp

  val data = (0 to 100).toList ++ List.fill(10)(200).scanLeft(2)((a, b) => a + b)

  print(data.zip(data.map(_ => now)))
  println(decayedValues.rmap(_._1.mean).runStream(data.toStream).toList)
}
