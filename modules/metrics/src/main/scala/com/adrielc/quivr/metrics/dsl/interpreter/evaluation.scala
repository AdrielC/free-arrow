package com.adrielc.quivr.metrics.dsl
package interpreter

import cats.data.{Kleisli, NonEmptyMap}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EvalErr
import com.adrielc.quivr.instances.all._
import com.adrielc.quivr.free.FreeArrow
import cats.implicits._
import com.adrielc.quivr.metrics.data.AccumMap
import com.adrielc.quivr.{AC, ACP, ~~>, analyzer}
import key._

object evaluation {
  type EvalOpMap[A, B]  = Kleisli[AccumMap[List[EvalOp[_, _]], EvalErr, *], A, B]
  type RunErr[A, B]     = Kleisli[EvalResult, A, B]

  def compileManyMetrics[A, B](fab: FreeArrow[ACP, EvalOp, A, B], describe: MetricKeyBuilder): A => NonEmptyMap[String, Either[EvalErr, B]] =
    fab.foldMap[EvalOpMap](new (EvalOp ~~> EvalOpMap) {
      def apply[C, D](fab: EvalOp[C, D]): EvalOpMap[C, D] = Kleisli { a => AccumMap.either(fab.pure[List], fab(a)) }
    }).run.rmap(_.sortedMap.toNel.map { case (k, v) => describe.summarize(k) -> v }.toNem)

  def compileSingleMetric[A, B](fab: FreeArrow[AC, EvalOp, A, B], describe: MetricKeyBuilder): A => (String, EvalResult[B]) = {
    val descr = describe.summarize(fab.analyze(analyzer[EvalOp].list))
    fab.foldMap[RunErr](new (EvalOp ~~> Kleisli[EvalResult, *, *]) {
      def apply[C, D](fab: EvalOp[C, D]): Kleisli[EvalResult, C, D] = Kleisli(fab(_))
    }).run.rmap(descr -> _)
  }
}
