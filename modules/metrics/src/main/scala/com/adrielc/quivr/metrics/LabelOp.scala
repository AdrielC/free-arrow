package com.adrielc.quivr.metrics

import cats.Functor
import cats.implicits._
import com.adrielc.quivr.data.{Pure, ~>|}
import com.adrielc.quivr.metrics.evaluable.{Results, ResultsWithEngagements, ResultsWithLabels}

import scala.math.{log}

sealed trait LabelOp[-A, +B] extends Product with Serializable {
  def apply(a: A): B
}
object LabelOp {

  case class LogDiscount(l: Int) extends LabelOp[Label, Option[Label]] {
    assert(l > 0)

    def apply(label: Label): Option[Label] =
      if(label < 0) None
      else if(label == 0) Some(0.0)
      else Some(log(label + 1) / log(l.toDouble))
  }

  case object Binary extends LabelOp[EngagementCounts, EngagementCounts] {
    def apply(a: EngagementCounts): EngagementCounts =
      a.mapValues(_.binarize)
  }

  case class CountOf(e: EngagementType) extends LabelOp[EngagementCounts, Double] {
    def apply(v1: EngagementCounts): Double =
      v1.countOf(e).toDouble
  }

  case class PercentOf(e: EngagementType, by: EngagementType) extends LabelOp[EngagementCounts, Double] {

    def apply(v1: EngagementCounts): Label = {
      val per = v1.countOf(by)
      if(per <= 0L) 0.0 else v1.countOf(e) / per.toDouble
    }
  }

  case class Weighted(weights: Map[EngagementType, Double]) extends LabelOp[EngagementCounts, Option[Double]] {

    def apply(v1: EngagementCounts): Option[Double] =
      weights.toList.foldMap { case (e, w) => v1.get(e).map(_ * w) }
  }

  case class Mapped[M[_]: Functor, A, B](labelOp: Labeler[A, B]) extends LabelOp[M[A], M[B]] {

    def apply(a: M[A]): M[B] = {
      val f = labelOp.foldMap(new Pure[LabelOp] {
        override def apply[C, D](fab: LabelOp[C, D]): C => D = fab(_)
      })
      a.map(f)
    }

    override def toString(): String = labelOp.analyze(new (LabelOp ~>| String) {
      def apply[C, D](fab: LabelOp[C, D]): String = fab.toString
    })
  }

  object free {
    import com.adrielc.quivr.free.FreeArrow.liftK

    def countOf(e: EngagementType) = liftK[LabelOp, ResultsWithEngagements, ResultsWithLabels](Mapped[Results, EngagementCounts, Double](liftK(CountOf(e))))

    sealed trait MissingLabels
    case object MissingLabels extends MissingLabels
  }
}