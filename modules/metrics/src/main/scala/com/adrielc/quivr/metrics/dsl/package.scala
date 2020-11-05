package com.adrielc.quivr
package metrics

import com.adrielc.quivr.free.{FA, FAP, FreeArrow}
import cats.implicits._
import FreeArrow.liftK
import cats.Order
import cats.data.{NonEmptyMap => Nem}
import com.adrielc.quivr.metrics.data.{Rank, ResultId, ResultRels}
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{EngagementToJudgement, EngagementToLabel}
import com.adrielc.quivr.metrics.dsl.evaluation.{EvalError, EvalOp}
import com.adrielc.quivr.metrics.dsl.key.SummarizeOps
import com.adrielc.quivr.metrics.ranking.{PartialRelevancies, Relevancies}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}

/**
 *
 * * * * Information Retrieval Evaluation Microlibrary
 *
 * Any datatype can be evaluated with this library, as long as represents a
 * sorted result set paired with a map of resultIds and engagement counts for any engagement type of your choosing
 *
 * This library allows you to simply describe how to compute your metrics and figures out how
 * to prepare, evaluate, and format that computation. Ideal use-cases are for bulk evaluation
 * jobs that need the following:
 * - consistent and descriptive keys for metrics
 * - debuggable results (i.e. metrics won't silently fail if zero results or labels are found),
 * - needs to be reusable/modular (i.e. same evaluation pipeline used for different datatypes)
 *
 */
package object dsl extends Syntax {

  /**
   * Sequential arrow composition
   *
   * [[>>]] is a subtype of [[+>]]
   *
   * {{{ arrAB >>> arrBC }}}
    */
  type >>[A, B]     = FA[EvalOp, A, B]

  /**
   * Independent arrow combination
   *
   * [[+>]] is a supertype of [[>>]]
   *
   * * {{{ arrAB <+> arrAB }}}
   */
  type +>[A, B]     = FAP[EvalOp, A, B]

  type EvalResult[+A] = Either[EvalError, A]

  object label {

    import engagement._

    /**
     * derive continuous relevance labels from result engagenments pertaining to [[A]]
     * **/
    def from[A]: EngagementLabelBuilder[A] = new EngagementLabelBuilder[A]

    /**
     * Count the number of engagements of type [[E]]
     * {{{ count(Clicks) + count(Purchases) }}}
     */
    def of[E](e: E): Labeler[E] = Labeler.countOf(e)

    /** {{{ ifThen(count(Clicks) === 0, -1) | ifThen(count(CartAdds) > 100, count(Purchases)) }}}  **/
    def ifThen[E, A](i: Judge[E], t: A)(implicit L: A LabelFor E): Labeler[E] =
      engagement.Labeler.ifThen(i, t.labeler)

    /**
     * Sum all of the counts for each sub expression
     * {{{ sum(count(Clicks), count(Purchases), count(CartAdds)*1.5) }}}
     */
    def sum[E](e: Labeler[E], es: Labeler[E]*): Labeler[E] =
      es.foldLeft(e)(_ + _)

    def weightedSum[E, A](w: (Labeler[E], A), ws: (Labeler[E], A)*)
                         (implicit E: A LabelFor E): Labeler[E] =
      sum(w._1 * w._2, ws.map { case (e, w) => e * w}:_*)

    /**
     * Implicitly available on any [[Labeler]]
     */
    class LabelerOps[E](private val exp: Labeler[E]) extends AnyVal {
      // compose with other Expressions

      // arithmetic
      def +[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.sum(exp, other.labeler)
      def *[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.times(exp, other.labeler)
      def /[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.div(exp, other.labeler)

      // returns a labeler that must satisfy the expression
      def filter: LabelerFilterOps[E] = new LabelerFilterOps(exp)

      // convert to a judgement if the label satisfies the predicates below
      def <=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.ltEq(exp, other.labeler)
      def >=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.gtEq(exp, other.labeler)
      def >[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.gt(exp, other.labeler)
      def <[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.lt(exp, other.labeler)
      def ===[B: LabelFor[*, E]](other: B)  : Judge[E] = Judge.isEq(exp, other.labeler)

      // fall back on other labeler if no valid labels can be found
      def |[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.or(exp, other.labeler)

      // makes this labeler dependent on the successful labeling on an other, then add the labels
      def &&[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.and(exp, other.labeler)

      /**
       * interpret this expression as a transformation of result engagements of type [[E]] to result labels of type [[A]]
       *
       * same as [[label.from]] except it is for individual labelers
       */
      def from[A: Engagements[*, E] : Results]: A >> ResultRels =
        FA.liftK[EvalOp, A, ResultRels](EngagementToLabel[A, E](exp))

      def runMap[A: Engagements[*, E]](a: A): Map[ResultId, Option[Double]] = {
        val f = interpreter.engagemement.label.labelerCompiler(exp)
        a.engagementCounts.mapValues(f.run)
      }

      def run[A: Engagements[*, E]: Results](a: A): Option[ResultRels] =
        from[A].run(a).toOption
    }

    class LabelerFilterOps[E](private val exp: Labeler[E]) extends AnyVal {
      def <=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.ltEq(exp, other.labeler)
      def >=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.gtEq(exp, other.labeler)
      def >[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.gt(exp, other.labeler)
      def <[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.lt(exp, other.labeler)
      def ===[B: LabelFor[*, E]](other: B): Labeler[E] = Labeler.isEq(exp, other.labeler)
    }
  }

  object judge {

    /** derive binary relevance judgements from result engagenments pertaining to [[A]] **/
    def from[A]: EngagementJudgeBuilder[A] = new EngagementJudgeBuilder[A]

    def any[E](e: E): Judge[E] =
      engagement.Judge.gt(Labeler.countOf(e), Labeler.value(0))

    /**
     * Implicitly available on any [[Labeler]], [[Int]], and [[Double]]
     */
    class JudgementOps[E](private val exp: Judge[E]) extends AnyVal {

      def |(other: Judge[E])  : Judge[E] = Judge.or(exp, other)

      def &&(other: Judge[E]) : Judge[E] = Judge.and(exp, other)

      // convert to labeler that runs if this predicate succeeds
      def ->>[B:engagement. LabelFor[*, E]](b: B): Labeler[E] = Labeler.ifThen(exp, b.labeler)

      /**
       * interpret this Expression as a function from Engagements of type [[E]] to a ground truth set for [[A]]
       */
      def from[A: Engagements[*, E] : Results]: A >> ResultRels =
        FA.liftK[EvalOp, A, ResultRels](EngagementToJudgement[A, E](exp))

      def run[A: Engagements[*, E]: Results](a: A): Option[ResultRels] =
        from[A].run(a).toOption
    }
  }


  // filters all downstream operations to K
  object atK {

    def apply[A: AtK](k: Rank): A >> A =
      liftK(EvalOp.K[A](k))

    def apply[A: AtK](k: Rank, ks: Rank*): A +> A =
      FreeArrow.plus(dsl.atK[A](k), ks.map(k => dsl.atK[A](k)):_*)
  }


  // compute metric
  object eval {
    import EvalOp.{Ndcg, QMeasure, FScore, Precision, AveragePrecision, ReciprocalRank, RPrecision, Recall}
    import function._

    def apply[A](m: A +> Double, ms: A +> Double*): A +> Double =
      FA.plus(m, ms:_*)

    def ndcg[A: Relevancies]: A >> Double =
      ndcgWithGain(gain.pow2)

    def qMeasure[A: PartialRelevancies](b: Double): A >> Double =
      FA.liftK(QMeasure(b))

    def ndcgWithGain[A: Relevancies](g: GainFn): A >> Double =
      FA.liftK(Ndcg(g, discount.log2))

    def fScore[A: RelevanceCounts]: A >> Double =
      FA.liftK(FScore[A])

    def recall[A: RelevanceCounts]: A >> Double =
      FA.liftK(Recall[A])

    def precision[A: TruePositiveCount]: A >> Double =
      FA.liftK(Precision[A])

    def averagePrecision[A: PartialRelevancies]: A >> Double =
      FA.liftK(AveragePrecision[A])

    def reciprocalRank[A: PartialRelevancies]: A >> Double =
      FA.liftK(ReciprocalRank[A])

    def rPrecision[A: Relevancies]: A >> Double =
      FA.liftK(RPrecision[A])
  }

  implicit class EvalOps[A, B](private val fab: FreeArrow[AR, EvalOp, A, B]) extends AnyVal {

    def run: A => EvalResult[B] =
      runWithKey.rmap(_._2)

    def runWithKey: A => (String, EvalResult[B]) =
      compile(interpreter.key.defaultKeyBuilder)

    def compile[M: Order](describeOps: SummarizeOps[EvalOp, M]): A => (M, EvalResult[B]) =
      interpreter.evaluation.compileSingleMetric(fab, describeOps)
  }

  implicit class EvalPlusOps[A, B](private val fab: FreeArrow[AP, EvalOp, A, B]) extends AnyVal {

    def run: A => Nem[String, EvalResult[B]] =
      compile(interpreter.key.defaultKeyBuilder)

    def compile[M: Order](describeOps: SummarizeOps[EvalOp, M]): A => Nem[M, EvalResult[B]] =
      interpreter.evaluation.compileManyMetrics(fab, describeOps)
  }
}

package dsl {

  import cats.data.NonEmptyList
  import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{EngagementToJudgement, EngagementToLabel}
  import com.adrielc.quivr.metrics.result.EngagedResults

  private[dsl] sealed trait ArrowFactory[F[_, _],  A, B] {
    type G[_]
    type TC[_]

    final def apply[E](e: G[E])(implicit E: TC[E]): FA[F, A, B] =
      FA.liftK(create(e))

    final def apply[E](e: G[E], es: G[E]*)(implicit E: TC[E]): FAP[F, A, B] =
      FA.plus(NonEmptyList(apply(e), es.map(apply(_)).toList))

    def create[E](e: G[E])(implicit E: TC[E]): F[A, B]
  }
  class EngagementJudgeBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, ResultRels] {
    type G[a] = Judge[a]; type TC[a] = EngagedResults[A, a]
    def create[E](e: Judge[E])(implicit E: EngagedResults[A, E]): EvalOp[A, ResultRels] = EngagementToJudgement(e)
  }
  class EngagementLabelBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, ResultRels] {
    type G[a] = Labeler[a]; type TC[a] = EngagedResults[A, a]
    def create[E](e: Labeler[E])(implicit E: EngagedResults[A, E]): EvalOp[A, ResultRels] = EngagementToLabel(e)
  }
  private[dsl] trait Syntax extends ExprSyntax0 with key.KeyBuilderSyntax {
    implicit def toRecOps[A](a: A): engagement.RecOps[A] = new engagement.RecOps(a)
    implicit def toJudgeOps[A](a: Judge[A]): dsl.judge.JudgementOps[A] = new judge.JudgementOps(a)
    implicit def toLabelerOps[A](a: Labeler[A]): dsl.label.LabelerOps[A] = new label.LabelerOps(a)
  }
  private[dsl] trait ExprSyntax0 {
    implicit def liftToLabelerOps[A](a: A)(implicit E: engagement.LabelFor[A, A]): label.LabelerOps[A] = new label.LabelerOps(a.labeler)
  }
}