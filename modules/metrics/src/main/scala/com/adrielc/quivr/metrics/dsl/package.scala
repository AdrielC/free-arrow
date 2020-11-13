package com.adrielc.quivr.metrics

import com.adrielc.quivr.free.{FA, FAP, FreeArrow}
import cats.data.{NonEmptyList, NonEmptyMap}
import com.adrielc.quivr.metrics.dsl.evaluation.{EvalError, EvalOp}
import cats.implicits._
import com.adrielc.quivr.metrics.data.EngagedResults
import com.adrielc.quivr.{ACP, AP, AR}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp.{EngagementToJudgement, EngagementToLabel}
import com.adrielc.quivr.metrics.dsl.label.LabelerFilterOps
import com.adrielc.quivr.metrics.result.AtK
import eu.timepit.refined.types.all.PosInt


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
package object dsl {
  import engagement._
  import function._

  /**
   * Sequential arrow composition
   *
   * [[>>]] is a subtype of [[+>]]
   *
   * {{{ arrAB >>> arrBC }}}
    */
  type >>[A, B] = FA[EvalOp, A, B]

  /**
   * Independent arrow combination
   *
   * [[+>]] is a supertype of [[>>]]
   *
   * * {{{ arrAB <+> arrAB }}}
   */
  type +>[A, B] = FAP[EvalOp, A, B]

  type EngRes[E] = EngagedResults[Map[E, Int]]

  type EvalResult[+A] = Either[EvalError, A]

  object label {

    def apply[E](e: Labeler[E]): EngRes[E] >> ResultRels =
      FA.liftK(EngagementToLabel(e): EvalOp[EngRes[E], ResultRels])

    def apply[E](e: Labeler[E], es: Labeler[E]*): EngRes[E] +> ResultRels =
      FA.plus(NonEmptyList(apply(e), es.map(apply).toList))

    /**
     * Count the number of engagements of type [[E]]
     * {{{ count(Clicks) + count(Purchases) }}}
     */
    def count[E](e: E): Labeler[E] = Labeler.count(e)

    /** {{{ ifThen(count(Clicks) === 0, -1) | ifThen(count(CartAdds) > 100, count(Purchases)) }}}  **/
    def ifThen[E, A](i: Judge[E], t: A)(implicit L: A LabelFor E): Labeler[E] =
      dsl.engagement.Labeler.as(i, t.labeler)

    /**
     * Sum all of the counts for each sub expression
     * {{{ sum(count(Clicks), count(Purchases), count(CartAdds)*1.5) }}}
     */
    def sum[E](e: Labeler[E], es: Labeler[E]*): Labeler[E] =
      es.foldLeft(e)(_ + _)

    def weightedSum[E, A](w: (Labeler[E], A), ws: (Labeler[E], A)*)
                         (implicit E: A LabelFor E): Labeler[E] =
      sum(w._1 * w._2, ws.map { case (e, w) => e * w}:_*)

    class LabelerFilterOps[E](exp: Labeler[E]) {
      def <=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.equiv(double.<=, exp, other.labeler)
      def >=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.equiv(double.>=, exp, other.labeler)
      def >[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.equiv(double.>, exp, other.labeler)
      def <[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.equiv(double.<, exp, other.labeler)
      def ===[B: LabelFor[*, E]](other: B): Labeler[E] = Labeler.equiv(double.===, exp, other.labeler)
    }
  }

  // compose with other Expressions
  implicit class LabelerOps[E](lab: Labeler[E]) {

    // arithmetic
    def +[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.sum(lab, other.labeler)
    def *[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.mult(lab, other.labeler)
    def /[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.div(lab, other.labeler)

    // returns a labeler that must satisfy the expression
    def filter: LabelerFilterOps[E] = new label.LabelerFilterOps(lab)

    // convert to a judgement if the label satisfies the predicates below
    def <=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.equiv(double.<=, lab, other.labeler)
    def >=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.equiv(double.>=, lab, other.labeler)
    def >[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.equiv(double.>, lab, other.labeler)
    def <[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.equiv(double.<, lab, other.labeler)
    def ===[B: LabelFor[*, E]](other: B)  : Judge[E] = Judge.equiv(double.===, lab, other.labeler)

    // fall back on other labeler if no valid labels can be found
    def |[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.or(lab, other.labeler)

    // makes this labeler dependent on the successful labeling on an other, then add the labels
    def &[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.and(lab, other.labeler)

    /**
     * interpret this expression as a transformation of result engagements of type [[E]] to result labels of type [[EngagedResults]]
     *
     * same as [[dsl.label.apply]] except it is for individual labelers
     */
    def liftA: EngRes[E] >> ResultRels =
      FA.liftK[EvalOp, EngRes[E], ResultRels](EngagementToLabel[E](lab))

    def run: Map[E, Int] => Option[Double] = interpreter.engagemement.label.labelerCompiler(lab).run
  }

  object judge {

    def any[E](e: E): Judge[E] =
      Labeler.count(e) > 0

    def apply[E](e: Judge[E]): EngRes[E] >> ResultRels =
      FA.liftK(EngagementToJudgement(e): EvalOp[EngRes[E], ResultRels])

    def apply[E](e: Judge[E], es: Judge[E]*): EngRes[E] +> ResultRels =
      FA.plus(NonEmptyList(apply(e), es.map(apply).toList))
  }

  implicit class JudgementOps[E](exp: Judge[E]) {

    def |(other: Judge[E]): Judge[E] =
      Judge.or(exp, other)

    def &(other: Judge[E]): Judge[E] =
      Judge.and(exp, other)

    // Postfix Alias for `as`
    // converts this Judge[E] to a Label[E]
    def ->>[B](b: B)(implicit L: B LabelFor E): Labeler[E] =
      Labeler.as(exp, b.labeler)

    /**
     * interpret this Expression as a function from Engagements of type [[E]] to a ground truth set for [[A]]
     */
    def liftA: EngRes[E] >> ResultRels =
      FA.liftK[EvalOp, EngRes[E], ResultRels](EngagementToJudgement[E](exp))

    def run: Map[E, Int] => Option[Boolean] =
      interpreter.engagemement.judge.judgeCompiler(exp).run
  }


  // filters all downstream operations to K
  object atK {

    def apply[A: AtK](k: PosInt): A >> A =
      FA.liftK(EvalOp.K[A](k))

    def apply[A: AtK](k: PosInt, ks: PosInt*): A +> A =
      FA.plus(dsl.atK[A](k), ks.map(k => dsl.atK[A](k)):_*)
  }

  // compute metric
  object eval {
    import EvalOp.Metric.{Ndcg, QMeasure, FScore, Precision, AveragePrecision, ReciprocalRank, RPrecision, Recall}

    def apply[A](m: ResultRels +> Double, ms: ResultRels +> Double*): ResultRels +> Double = FA.plus(m, ms:_*)

    val ndcg: ResultRels >> Double = ndcgWithGain(gain.pow2)

    def qMeasure(b: Double = 1): ResultRels >> Double = FA.liftK(QMeasure(b))

    def ndcgWithGain(g: GainFn): ResultRels >> Double = FA.liftK(Ndcg(g, discount.log2))

    val fScore: ResultRels >> Double = FA.liftK(FScore)

    val recall: ResultRels >> Double = FA.liftK(Recall)

    val precision: ResultRels >> Double = FA.liftK(Precision)

    val averagePrecision: ResultRels >> Double = FA.liftK(AveragePrecision)

    val reciprocalRank: ResultRels >> Double = FA.liftK(ReciprocalRank)

    val rPrecision: ResultRels >> Double = FA.liftK(RPrecision)
  }

  implicit class EvalOps[A, B](private val fab: FreeArrow[AR, EvalOp, A, B]) extends AnyVal {

    def run: A => (String, Option[B]) =
      interpreter.evaluation.compileSingle(fab, interpreter.key.defaultKeyBuilder)
  }
  implicit class EvalPlusOps[R[f[_, _]] >: ACP[f] <: AP[f], A, B](private val fab: FreeArrow[R, EvalOp, A, B]) extends AnyVal {

    def run: A => NonEmptyMap[String, EvalResult[B]] =
      interpreter.evaluation.compileManyMetrics(fab, interpreter.key.defaultKeyBuilder)
  }
}