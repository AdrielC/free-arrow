package com.adrielc.quivr
package metrics

import com.adrielc.quivr.free.{FA, FAP, FreeArrow}
import cats.data.{NonEmptyList, NonEmptyMap => Nem}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{EngagementToJudgement, EngagementToLabel, LabelsToJudgement}
import com.adrielc.quivr.metrics.dsl.evaluation.{EvalError, EvalOp}
import cats.implicits._
import com.adrielc.quivr.metrics.data.Labeled.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.dsl.label.LabelerFilterOps
import com.adrielc.quivr.metrics.result.{AtK, Engagements, ResultLabels, Results}
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

  type EvalResult[+A] = Either[EvalError, A]


  /**
   * Identity eval arrow
   * Helpful when creating arrows where the types can be
   * better inferred when specifying an input
   */
  def ^[A]: A >> A = FA[A]

  object label {

    def apply[A, E](e: Labeler[E])(implicit E: Engagements[A, E]): A >> WithLabels[A] =
      FA.liftK(EngagementToLabel(e): EvalOp[A, WithLabels[A]])

    def apply[A, E](e: Labeler[E], es: Labeler[E]*)(implicit E: Engagements[A, E]): A +> WithLabels[A] =
      FA.plus(NonEmptyList(apply(e), es.map(apply(_)).toList))

    /**
     * derive continuous relevance labels from result engagenments pertaining to [[A]]
     * **/
    def from[A]: PartiallyAppliedLabeler[A] = new PartiallyAppliedLabeler[A]

    final class PartiallyAppliedLabeler[A] private[dsl] {

      def apply[E](e: Labeler[E])(implicit E: Engagements[A, E]): A >> WithLabels[A] =
        label(e)

      def apply[E](e: Labeler[E], es: Labeler[E]*)(implicit E: Engagements[A, E]): A +> WithLabels[A] =
        label(e, es:_*)
    }

    /**
     * Count the number of engagements of type [[E]]
     * {{{ count(Clicks) + count(Purchases) }}}
     */
    def of[E](e: E): Labeler[E] = Labeler.countOf(e)

    /** {{{ ifThen(count(Clicks) === 0, -1) | ifThen(count(CartAdds) > 100, count(Purchases)) }}}  **/
    def ifThen[E, A](i: Judge[E], t: A)(implicit L: A LabelFor E): Labeler[E] =
      engagement.Labeler.as(i, t.labeler)

    /**
     * Sum all of the counts for each sub expression
     * {{{ sum(count(Clicks), count(Purchases), count(CartAdds)*1.5) }}}
     */
    def sum[E](e: Labeler[E], es: Labeler[E]*): Labeler[E] =
      es.foldLeft(e)(_ + _)

    def weightedSum[E, A](w: (Labeler[E], A), ws: (Labeler[E], A)*)
                         (implicit E: A LabelFor E): Labeler[E] =
      sum(w._1 * w._2, ws.map { case (e, w) => e * w}:_*)

    class LabelerFilterOps[E](private val exp: Labeler[E]) extends AnyVal {
      def <=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.equiv(double.<=, exp, other.labeler)
      def >=[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.equiv(double.>=, exp, other.labeler)
      def >[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.equiv(double.>, exp, other.labeler)
      def <[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.equiv(double.<, exp, other.labeler)
      def ===[B: LabelFor[*, E]](other: B): Labeler[E] = Labeler.equiv(double.===, exp, other.labeler)
    }
  }

  // compose with other Expressions
  implicit class LabelerOps[E](private val lab: Labeler[E]) extends AnyVal {

    // arithmetic
    def +[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.sum(lab, other.labeler)
    def *[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.times(lab, other.labeler)
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
    def &&[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.and(lab, other.labeler)

    /**
     * interpret this expression as a transformation of result engagements of type [[E]] to result labels of type [[A]]
     *
     * same as [[dsl.label.apply]] except it is for individual labelers
     */
    def from[A: Engagements[*, E]]: A >> WithLabels[A] =
      FA.liftK[EvalOp, A, WithLabels[A]](EngagementToLabel[A, E](lab))

    def engsToLabels[A](a: A)(implicit E: Engagements[A, E]): Map[ResultId, Option[Double]] = {
      val f = interpreter.engagemement.label.labelerCompiler(lab)
      E.engagements(a).mapValues(f.run)
    }

    def labelResults[A: Engagements[*, E]](a: A): Option[WithLabels[A]] =
      from[A].run(a)._2
  }

  object judge {

    def apply[A, E](e: Judge[E])(implicit E: Engagements[A, E]): A >> WithGroundTruth[A] =
      FA.liftK(EngagementToJudgement(e): EvalOp[A, WithGroundTruth[A]])

    def apply[A, E](e: Judge[E], es: Judge[E]*)(implicit E: Engagements[A, E]): A +> WithGroundTruth[A] =
      FA.plus(NonEmptyList(apply(e), es.map(apply(_)).toList))

    /**
     *
     * derive binary relevance judgements from result engagenments pertaining to [[A]]
     *
     * {{{
     * val clicks = label.count(Click)
     * val single: MyResults >> ResultRels = judge[MyResults](clicks > 0)
     * val multiple: MyResults +> ResultRels = judge[MyResults](clicks > 0, clicks > 5, clicks > 10)
     * }}}
     *
     * @tparam A The type from which to extract engagements of type [[E]]
     * @return an arrow with [[A]] as input and [[ResultRels]] as output
     */
    def from[A]: EngagementJudgeBuilder[A] = new EngagementJudgeBuilder[A]

    final class EngagementJudgeBuilder[A] private[dsl] {

      def apply[E](e: Judge[E])(implicit E: Engagements[A, E]): A >> WithGroundTruth[A] =
        judge(e)

      def apply[E](e: Judge[E], es: Judge[E]*)(implicit E: Engagements[A, E]): A +> WithGroundTruth[A] =
        judge(e, es:_*)
    }

    def any[E](e: E): Judge[E] = Labeler.countOf(e) > 0

    def labels[A]: JudgeLabelOps[A] = new JudgeLabelOps[A]

    class JudgeLabelOps[A] private[dsl] {
      def <=(d: Double)(implicit R: ResultLabels[A]) : A >> WithGroundTruth[A] = FA.liftK(LabelsToJudgement[A](double.<=, d): EvalOp[A, WithGroundTruth[A]])
      def >=(d: Double)(implicit R: ResultLabels[A]) : A >> WithGroundTruth[A] = FA.liftK(LabelsToJudgement[A](double.>=, d): EvalOp[A, WithGroundTruth[A]])
      def >(d: Double)(implicit R: ResultLabels[A])  : A >> WithGroundTruth[A] = FA.liftK(LabelsToJudgement[A](double.>, d): EvalOp[A, WithGroundTruth[A]])
      def <(d: Double)(implicit R: ResultLabels[A])  : A >> WithGroundTruth[A] = FA.liftK(LabelsToJudgement[A](double.<, d): EvalOp[A, WithGroundTruth[A]])
      def ===(d: Double)(implicit R: ResultLabels[A]): A >> WithGroundTruth[A] = FA.liftK(LabelsToJudgement[A](double.===, d): EvalOp[A, WithGroundTruth[A]])
    }
  }

  implicit class JudgementOps[E](private val exp: Judge[E]) extends AnyVal {

    def |(other: Judge[E])  : Judge[E] = Judge.or(exp, other)

    def &&(other: Judge[E]) : Judge[E] = Judge.and(exp, other)

    // convert to labeler that runs if this predicate succeeds
    def ->>[B:engagement. LabelFor[*, E]](b: B): Labeler[E] = Labeler.as(exp, b.labeler)

    /**
     * interpret this Expression as a function from Engagements of type [[E]] to a ground truth set for [[A]]
     */
    def from[A: Engagements[*, E] : Results]: A >> WithGroundTruth[A] =
      FA.liftK[EvalOp, A, WithGroundTruth[A]](EngagementToJudgement[A, E](exp))

    def run[A: Engagements[*, E]: Results](a: A): Option[WithGroundTruth[A]] =
      from[A].run(a)._2
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
    import EvalOp.MetricOp.{Ndcg, QMeasure, FScore, Precision, AveragePrecision, ReciprocalRank, RPrecision, Recall}
    import ranking.{ResultRelevancies, RankedRelevancies}
    import retrieval.{RelevanceCount, TruePositiveCount}

    def apply[A](m: A +> Double, ms: A +> Double*): A +> Double =
      FA.plus(m, ms:_*)

    def ndcg[A: ResultRelevancies]: A >> Double =
      ndcgWithGain(gain.pow2)

    def qMeasure[A: RankedRelevancies](b: Double): A >> Double =
      FA.liftK(QMeasure(b))

    def ndcgWithGain[A: ResultRelevancies](g: GainFn): A >> Double =
      FA.liftK(Ndcg(g, discount.log2))

    def fScore[A: RelevanceCount]: A >> Double =
      FA.liftK(FScore[A])

    def recall[A: RelevanceCount]: A >> Double =
      FA.liftK(Recall[A])

    def precision[A: TruePositiveCount]: A >> Double =
      FA.liftK(Precision[A])

    def averagePrecision[A: RankedRelevancies]: A >> Double =
      FA.liftK(AveragePrecision[A])

    def reciprocalRank[A: RankedRelevancies]: A >> Double =
      FA.liftK(ReciprocalRank[A])

    def rPrecision[A: ResultRelevancies]: A >> Double =
      FA.liftK(RPrecision[A])
  }

  implicit class EvalOps[A, B](private val fab: FreeArrow[AR, EvalOp, A, B]) extends AnyVal {

    def run: A => (String, Option[B]) =
      interpreter.evaluation.compileSingle(fab, interpreter.key.defaultKeyBuilder)
  }
  implicit class EvalPlusOps[A, B](private val fab: FreeArrow[AP, EvalOp, A, B]) extends AnyVal {

    def run: A => Nem[String, EvalResult[B]] =
      interpreter.evaluation.compileManyMetrics(fab, interpreter.key.defaultKeyBuilder)
  }
}