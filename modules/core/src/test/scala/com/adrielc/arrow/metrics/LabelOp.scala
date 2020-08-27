package com.adrielc.arrow.metrics

import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._
import com.adrielc.arrow.metrics.evaluable.{LabelledIndexes, ResultsWithEngagements}

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
      v1.map { case (e, n) => (e, w.getOrElse(e, 0.0) * n) }
  }

  case object Pow2 extends LabelOp[Double, Double] {

    def apply(v1: Double): Double =
      math.pow(2, v1) - 1
  }

  case class Mapped[M[_]: Functor, A, B](labelOp: FreeLabel[A, B]) extends LabelOp[M[A], M[B]] {

    def apply(v1: M[A]): M[B] = {
      val f = labelOp.fold[Function1]
      v1.map(f)
    }
  }


  sealed trait MissingLabels
  case object MissingLabels extends MissingLabels


  object free {
    import com.adrielc.arrow.free.FreeA.{liftK, lift}

    val pow2                : FreeLabel[Double, Double] = liftK(Pow2)
    def binary[A: Numeric]  : FreeLabel[A, A]     = liftK(Binary[A])
    def countOf(e: EngagementType)  : FreeLabel[EngagementCounts, Count]     = liftK(CountOf(e))

    implicit class LabelOps[A, B](private val freeLabel: FreeLabel[A, B]) {
      private lazy val f = freeLabel.fold[Function1]
      def apply(a: A): B = f(a)

      def mapped[M[_]: Functor]: FreeLabel[M[A], M[B]] = liftK[LabelOp, M[A], M[B]](Mapped[M, A, B](freeLabel))
    }

    implicit class EngLabelOps(private val engToLabel: FreeLabel[EngagementCounts, Double]) {

      def forEngagedResults: FreeLabel[ResultsWithEngagements, Either[MissingLabels, LabelledIndexes]] = {
        ((lift((_: ResultsWithEngagements).engagements) >>> engToLabel.mapped[Map[ResultId, *]]) &&&
          lift((_: ResultsWithEngagements).results)) >>^ {
          case (labels, results) =>
            NonEmptyList.fromList(labels.map { case (id, label) => (results.toList.indexOf(id) + 1) -> label }.toList)
              .map(LabelledIndexes(_))
              .toRight(MissingLabels)
        }
      }
    }
  }
}