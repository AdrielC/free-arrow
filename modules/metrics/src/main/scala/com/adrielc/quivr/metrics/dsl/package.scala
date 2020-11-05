package com.adrielc.quivr
package metrics

import com.adrielc.quivr.free.{FA, FAP, FreeArrow}
import cats.implicits._
import FreeArrow.liftK
import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem}
import com.adrielc.quivr.metrics.data.{Rank, ResultId, ResultRels}
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp.{EngagementToJudgement, EngagementToLabel}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.MetricOp.{AveragePrecision, FScore, Ndcg, Precision, QMeasure, RPrecision, Recall, ReciprocalRank}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EvalErr
import com.adrielc.quivr.metrics.dsl.interpreter.key.MetricKeyBuilder
import com.adrielc.quivr.metrics.ranking.{PartialRelevancies, Relevancies}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, Results}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}
import eu.timepit.refined.cats._

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
 * * * * Short Example:
 *
 * {{{
 *
 * sealed trait Eng // user defined engagement domain
 * case object Clicks extends Eng
 *
 * type MyResults = (NonEmptyList[ResultId], Map[ResultId, Map[Eng, Int]]) // user defined results type
 *
 * val results: MyResults = (
 *   NonEmptyList.of(1L, 2L to 59L:_*), // results
 *   Map(2L -> Map((Clicks: Eng) -> 1)) // engagement counts
 * )
 *
 * import dsl._
 *
 * val evaluation =
 *   label.count.of(Clicks: Eng).from[MyResults] >>> // count clicks to determine relevance labels
 *     atK(10, 60) >++                               // compute downstream metrics for each K
 *     (eval.ndcg, eval.reciprocalRank)              // compute each metric
 *
 * val metrics = evaluation.run(results)
 *
 * metrics == NonEmptyMap.of(
 *   "label(clicks).mrr.@10" -> Right(0.5),
 *   "label(clicks).ndcg.@10" -> Right(0.6309297535714574),
 *   "label(clicks).@60" -> Left(KGreaterThanMax: EvalErr) // metric key stops building on error so Errors aren't repeated for all downstream metric combinations
 * )
 * }}}
 *
 *
 * * * * Walkthrough
 * {{{
 *
 * // your own class
 * case class MyClass(clicks: Option[PosInt], ...)
 * object MyClass {
 *  sealed trait Eng
 *  case object Clicks extends Eng
 *  case object Reviews extends Eng
 *  implicit val engagementsInstance: quivr.metrics.Engagements[MyClass, Eng] = ???
 * }
 *
 * import dsl._
 *
 * // define building blocks for evaluation
 *
 * The four building blocks are
 * 1) Labeler[E]  : Defines how to create continuous/graded labels from your data
 * 2) Judge[E]    : Defines how to create binary relevance judgements from your data
 * 3) A >> B      : Represents a single step in your pipeline that inputs [[A]] and outputs [[B]]
 * 4) A +> B      : Represents multiple [[[A >> B]]] arrows that have been batched together, still having inputs [[A]] and outputs [[B]]
 *
 * // define how you want to extract your labels
 * val countClicks    = label.count.of(Clicks)    : Labeler[Eng]
 * val countPurchases = label.count.of(Reviews)   : Labeler[Eng]
 * val anyClicks      = judge.count.any(Clicks)   : Judge[Eng]
 * val anyPurchases   = judge.count.any(Purchases): Judge[Eng]
 *
 * // specify which datatype will be required for input
 *
 * val singleLabeler: MyClass >> WithLabels[MyClass]  = countClicks.from[MyClass]   // for one labeler
 *
 * val manyLabelers: MyClass +> WithLabels[MyClass]   = label.count.from[MyClass](  // or for many labelers
 *  countClicks,
 *  countPurchases,
 *  countClicks + (countPurchases * 5)
 * )
 *
 * // specify values of k to compute metrics by
 * val atKs: WithLabels[MyClass] +> WithLabels[MyClass] = atK(10, 20, 30, 40, 60)
 *
 * // Build an evaluator: Compose via `>>>`, combine via `<+>`, and compose/combine in a single step via `>++`
 *
 * val labelEval = (manyLabelers <+> singleLabeler) >>> atKTen >>> eval.ndcg
 *
 * val judgeEval = anyClicks >>> atK(5, 10, 40, 90) >++ (eval.precision, eval.recall, eval.averagePrecision)
 *
 * val fullEvaluation: MyClass +> Double = labelEval <+> judgeEval
 *
 * // You can also map ordinary functions into the structure via `rmap`, `lmap`
 *
 * val legacyEvaluation: OtherClass +> Double = fullEvaluation.lmap((_: OtherClass).toMyclass)
 *
 * val metrics: NonEmptyMap[String, EvalResult[Double]] = legacyEvaluation.run(otherClass) // Each metric will be accounted for, failure (e.g. no results) or not
 *
 * val metric: (String, EvalResult[Double]) = (singleLabeler >>> eval.ndcg).run(myClass) // Or just compute one metric.
 *
 * }}}
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

 // A >> B is a subtype of A +> B

  type EvalResult[+A] = Either[EvalErr, A]

  object label {

    object count {
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
       * Implicitly available on any [[Labeler]], [[Int]], and [[Double]]
       */
      class LabelerOps[E](private val exp: Labeler[E]) extends AnyVal {
        // compose with other Expressions

        // arithmetic
        def +[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.sum(exp, other.labeler)
        def *[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.times(exp, other.labeler)
        def /[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.div(exp, other.labeler)

        // returns a labeler that must satisfy the expression
        def filter: LabelerFilterOps[E] = new LabelerFilterOps(exp)

        def <=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.ltEq(exp, other.labeler)
        def >=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.gtEq(exp, other.labeler)
        def >[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.gt(exp, other.labeler)
        def <[B: LabelFor[*, E]](other: B)    : Judge[E] = Judge.lt(exp, other.labeler)
        def ===[B: LabelFor[*, E]](other: B)  : Judge[E] = Judge.isEq(exp, other.labeler)


        def |[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.or(exp, other.labeler)
        def &&[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.and(exp, other.labeler)

        /**
         * interpret this expression as a transformation of result engagements of type [[E]] to result labels of type [[A]]
         *
         * same as [[count.from]] except it is for individual labelers
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
  }

  object judge {

    object count {

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

        def as[B: engagement.LabelFor[*, E]](b: B): Labeler[E] = dsl.label.count.ifThen(exp, b)
        def ->>[B:engagement. LabelFor[*, E]](b: B): Labeler[E] = dsl.label.count.ifThen(exp, b)

        /**
         * interpret this Expression as a function from Engagements of type [[E]] to a ground truth set for [[A]]
         */
        def from[A: Engagements[*, E] : Results]: A >> ResultRels =
          FA.liftK[EvalOp, A, ResultRels](EngagementToJudgement[A, E](exp))

        def run[A: Engagements[*, E]: Results](a: A): Option[ResultRels] =
          from[A].run(a).toOption
      }
    }
  }


  // filters all downstream operations to K
  object atK {

    def apply[A: AtK](k: Rank): A >> A =
      liftK(EvalOp.K[A](k))

    def apply[A: AtK](k: Rank, ks: Rank*): A +> A =
      FreeArrow.plus(dsl.atK[A](k), ks.map(k => dsl.atK[A](k)):_*)

    def apply[A: AtK, B](k: (Rank, A >> B), ks: (Rank, A >> B)*): A +> B =
      FreeArrow.plus(Nel(k, ks.toList).groupByNem(_._1).toNel.map { case (k, n) => atK(k) >>> FreeArrow.plus(n.map(_._2)) })
  }


  // compute metric
  object eval {
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
      compile()

    def compile(describeOps: MetricKeyBuilder = interpreter.key.defaultKeyBuilder): A => (String, EvalResult[B]) =
      interpreter.evaluation.compileSingleMetric(fab, describeOps)
  }

  implicit class EvalPlusOps[A, B](private val fab: FreeArrow[AP, EvalOp, A, B]) extends AnyVal {

    def run: A => Nem[String, EvalResult[B]] =
      compile()

    def compile(describeOps: MetricKeyBuilder = interpreter.key.defaultKeyBuilder): A => Nem[String, EvalResult[B]] =
      interpreter.evaluation.compileManyMetrics(fab, describeOps)
  }
}

package dsl {

  import cats.data.NonEmptyList

  private[dsl] sealed trait ArrowFactory[F[_, _],  A, B] {
    type G[_]
    type TC[_]

    final def apply[E](e: G[E])(implicit E: TC[E]): FA[F, A, B] =
      FA.liftK(create(e))

    final def apply[E](e: G[E], es: G[E]*)(implicit E: TC[E]): FAP[F, A, B] =
      FA.plus(NonEmptyList(apply(e), es.map(apply(_)).toList))

    def create[E](e: G[E])(implicit E: TC[E]): F[A, B]
  }

  trait PartiallyAppliedArrowBuilder[F[_, _], M[_]] {
    def apply[A]: ArrowFactory[F, A, M[A]]
  }
  class EngagementJudgeBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, ResultRels] {
    type G[a] = Judge[a]; type TC[a] = Engagements[A, a] with Results[A]
    def create[E](e: Judge[E])(implicit E: Engagements[A, E] with Results[A]): EvalOp[A, ResultRels] = EngagementToJudgement(e)
  }
  class EngagementLabelBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, ResultRels] {
    type G[a] = Labeler[a]; type TC[a] = Engagements[A, a] with Results[A]
    def create[E](e: Labeler[E])(implicit E: Engagements[A, E] with Results[A]): EvalOp[A, ResultRels] = EngagementToLabel(e)
  }
  class AtKBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, A] {
    type G[a] = Rank; type TC[a] = AtK[A]
    override def create[E](k: Rank)(implicit E: AtK[A]): EvalOp[A, A] = EvalOp.K[A](k)
  }
  private[dsl] trait Syntax extends ExprSyntax0 with key.KeyBuilderSyntax {
    implicit def toRecOps[A](a: A): engagement.RecOps[A] = new engagement.RecOps(a)
    implicit def toJudgeOps[A](a: Judge[A]): dsl.judge.count.JudgementOps[A] = new judge.count.JudgementOps(a)
    implicit def toLabelerOps[A](a: Labeler[A]): dsl.label.count.LabelerOps[A] = new label.count.LabelerOps(a)
  }
  private[dsl] trait ExprSyntax0 {
    implicit def liftToLabelerOps[A](a: A)(implicit E: engagement.LabelFor[A, A]): label.count.LabelerOps[A] = new label.count.LabelerOps(a.labeler)
  }
}