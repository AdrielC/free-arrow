package com.adrielc.arrow.metrics

import cats.Functor
import cats.implicits._
import com.adrielc.arrow.data.Pure

import scala.math.{abs, log}

sealed trait LabelOp[-A, +B] extends (A => B)
object LabelOp {

  case class LogDiscount(l: Int) extends LabelOp[Double, Double] {

    def apply(label: Label): Label =
      log(abs(label) + 1) / log(l.toDouble)
  }

  case class Binary[A: Numeric]() extends LabelOp[A, A] {
    def apply(v1: A): A =
      v1.binarize
  }

  case class CountOf(e: EngagementType) extends LabelOp[EngagementCounts, Long] {
    def apply(v1: EngagementCounts): ResultId =
      v1.countOf(e)
  }

  case class PercentOf(e: EngagementType, by: EngagementType) extends LabelOp[EngagementCounts, Double] {

    def apply(v1: EngagementCounts): Label = {
      val per = v1.countOf(by)
      if(per <= 0L) 0.0 else v1.countOf(e) / per.toDouble
    }
  }

  case class Weighted(w: Map[EngagementType, Double]) extends LabelOp[EngagementCounts, Map[EngagementType, Double]] {

    def apply(v1: EngagementCounts): Map[EngagementType, Label] =
      v1.map { case (e, n) => e ->  w.getOrElse(e, 0.0) * n }
  }

  case class Mapped[M[_]: Functor, A, B](labelOp: FreeLabel[A, B]) extends LabelOp[M[A], M[B]] {

    def apply(v1: M[A]): M[B] = {
      val f = labelOp.fold[Function1]
      v1.map(f)
    }
  }

  implicit lazy val pureLabel: Pure[LabelOp] = new Pure[LabelOp] { def apply[A, B](fab: LabelOp[A, B]): A => B = fab }
}