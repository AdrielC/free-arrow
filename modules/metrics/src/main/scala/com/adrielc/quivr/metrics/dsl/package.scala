package com.adrielc.quivr
package metrics

import com.adrielc.quivr.free.{FA, FAP, FreeArrow}
import cats.implicits._
import FreeArrow.liftK
import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem, NonEmptySet => Nes}
import com.adrielc.quivr.metrics.data.Judged.{WithGroundTruth, WithLabels}
import com.adrielc.quivr.metrics.data.{Rank, ResultId}
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp.{EngagementToJudgement, EngagementToLabel}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.MetricOp.{AveragePrecision, FScore, Ndcg, Precision, RPrecision, Recall, ReciprocalRank}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{BinaryRels, EvalErr}
import com.adrielc.quivr.metrics.dsl.interpreter.key.MetricKeyBuilder
import com.adrielc.quivr.metrics.ranking.{BinaryRelevance, GradedRelevance}
import com.adrielc.quivr.metrics.result.{AtK, Engagements, ResultLabels}
import com.adrielc.quivr.metrics.retrieval.{RelevanceCounts, TruePositiveCount}
import eu.timepit.refined.cats._

/**
 *
 * IR Evaluation DSL
 *
 *
 * example
 *
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
 * val clicks = dsl.eng.count(Clicks)                           : Expr[Eng]
 * val reviews = dsl.eng.count(Reviews)                         : Expr[Eng]
 *
 * val binaryclickLabels = !clicks.label[MyClass]                 : MyClass >>> WithLabels[MyClass]
 * val binaryReviewJudge = !reviews.judge[MyClass]                : MyClass >>> WithGroundTruth[Labeled]
 * val clickAndReview    = (clicks & reviews).judge[MyClass]      : MyClass >>> WithGroundTruth[Labeled]
 *
 * def atKTen       = atK(10)                                   : MyClass >>> MyClass
 * def atKThirty    = atK(10)                                   : MyClass >>> MyClass
 * def atKFifty     = atK(50)                                   : MyClass >>> MyClass
 *
 *
 * // Build an evaluator: Compose via `>>>`, combine via `<+>`, and both via `>++`
 *
 * val labelEval = (clickLabels <+> clickAndReview) >>> (atKTen <+> atKFifty) >>> eval.ndcg
 *
 * val judgeEval = binaryReview >>> atK(5, 10, 40, 90) >++ (eval.precision, eval.recall, eval.averagePrecision)
 *
 * // You can also map ordinary functions into the structure via `rmap`, `lmap`, and FreeArrow.lift
 *
 * def sendToKafka(v: Double, q: Query): Unit = ???
 *
 * import FreeArrow._
 *
 *
 * val engCache: Map[Query, MyClass] = ???
 *
 * def evaluateForQuery(q: Query): Query >>> Double =
 *  lift((q: Query) => engCache.getOrElse(q, MyClass.empty)) >>>
 *    (labelEval <+> judgeEval) >^
 *    (value => sendToKafka(value, query) )
 *
 * // Run the eval with `eval.run` which is implicitly in scope  when importing `quivr.metrics.dsl._`
 *
 * val myClass: MyClass = ???
 *
 * judgeEval.run(myClass)
 *
 *
 *
 *
 *
 * }}}
 *
 */
package object dsl extends Syntax {

  // Sequential/dependent arrow composition
  type >>[A, B]     = FA[EvalOp, A, B]

  // Horizontal/independent arrow composition
  type +>[A, B]     = FAP[EvalOp, A, B]

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

        def <=[B: LabelFor[*, E]](other: B)   : Judge[E] = engagement.Judge.ltEq(exp, other.labeler)
        def >=[B: LabelFor[*, E]](other: B)   : Judge[E] = Judge.gtEq(exp, other.labeler)
        def >[B: LabelFor[*, E]](other: B)    : Judge[E] = engagement.Judge.gt(exp, other.labeler)
        def <[B: LabelFor[*, E]](other: B)    : Judge[E] = engagement.Judge.lt(exp, other.labeler)
        def ===[B: LabelFor[*, E]](other: B)  : Judge[E] = engagement.Judge.isEq(exp, other.labeler)


        def |[B: LabelFor[*, E]](other: B)  : Labeler[E] = Labeler.or(exp, other.labeler)
        def &&[B: LabelFor[*, E]](other: B) : Labeler[E] = Labeler.and(exp, other.labeler)

        /**
         * interpret this expression as a transformation of result engagements of type [[E]] to result labels of type [[A]]
         */
        def from[A: Engagements[*, E]]: A >> WithLabels[A] =
          FA.liftK[EvalOp, A, WithLabels[A]](EngagementToLabel[A, E](exp))

        def run[A: Engagements[*, E]](a: A): Option[Nem[ResultId, Double]] =
          from[A].run(a).toOption.map(_.labels)
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
        def from[A: Engagements[*, E]]: A >> WithGroundTruth[A] =
          FA.liftK[EvalOp, A, WithGroundTruth[A]](EngagementToJudgement[A, E](exp))

        def run[A: Engagements[*, E]](a: A): Option[Nes[ResultId]] =
          from[A].run(a).toOption.map(_.groundTruth)
      }
    }


    // converts labels to relevance judgements
    object label {

      def aboveThreshold[A: ResultLabels](t: Int): A >> WithGroundTruth[A] =
        liftK(BinaryRels[A](t))

      def aboveThresholds[A] = new LabelJudgementsBuilder[A]

      def pos[A: ResultLabels]: A >> WithGroundTruth[A] =
        liftK(BinaryRels[A](0))
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

    def apply[A](m: A +> Double, ms: A +> Double*): A +> Double =
      FA.plus(m, ms:_*)

    def ndcg[A: GradedRelevance]: A >> Double =
      ndcgWithGain(gain.pow2)

    def ndcgWithGain[A: GradedRelevance](g: Gain): A >> Double =
      FA.liftK(Ndcg(g, discount.log2))

    def fScore[A: RelevanceCounts]: A >> Double =
      FA.liftK(FScore[A])

    def recall[A: RelevanceCounts]: A >> Double =
      FA.liftK(Recall[A])

    def precision[A: TruePositiveCount]: A >> Double =
      FA.liftK(Precision[A])

    def averagePrecision[A: BinaryRelevance]: A >> Double =
      FA.liftK(AveragePrecision[A])

    def reciprocalRank[A: BinaryRelevance]: A >> Double =
      FA.liftK(ReciprocalRank[A])

    def rPrecision[A: BinaryRelevance]: A >> Double =
      FA.liftK(RPrecision[A])
  }

  implicit class EvalOps[A, B](private val fab: FreeArrow[AR, EvalOp, A, B]) extends AnyVal {

    def relIfAbove(t: Int)(implicit R: ResultLabels[B]): A >> WithGroundTruth[B] =
      fab >>> dsl.judge.label.aboveThreshold[B](t)

    def relIfPositive(implicit R: ResultLabels[B]): A >> WithGroundTruth[B] =
      fab >>> dsl.judge.label.pos

    def run: A => EvalResult[B] =
      runWithKey.rmap(_._2)

    def runWithKey: A => (String, EvalResult[B]) =
      compile()

    def compile(describeOps: MetricKeyBuilder = interpreter.key.defaultKeyBuilder): A => (String, EvalResult[B]) =
      interpreter.evaluation.compileToFunction(fab, describeOps)
  }

  implicit class EvalPlusOps[A, B](private val fab: FreeArrow[AP, EvalOp, A, B]) extends AnyVal {

    def relIfAbove(t: Int)(implicit R: ResultLabels[B]): A +> WithGroundTruth[B] =
      fab >>> dsl.judge.label.aboveThreshold[B](t)

    def relIfPositive(implicit R: ResultLabels[B]): A +> WithGroundTruth[B] =
      fab >>> dsl.judge.label.pos

    def run: A => Nem[String, EvalResult[B]] =
      compile()

    def compile(describeOps: MetricKeyBuilder = interpreter.key.defaultKeyBuilder): A => Nem[String, EvalResult[B]] =
      interpreter.evaluation.compileToEvaluator(fab, describeOps)
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
  class EngagementJudgeBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, WithGroundTruth[A]] {
    override type G[a] = Judge[a]
    override type TC[a] = Engagements[A, a]
    def create[E](e: Judge[E])(implicit E: Engagements[A, E]): EvalOp[A, WithGroundTruth[A]] = EngagementToJudgement(e)
  }

  class EngagementLabelBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, WithLabels[A]] {
    override type G[a] = Labeler[a]
    override type TC[a] = Engagements[A, a]
    def create[E](e: Labeler[E])(implicit E: Engagements[A, E]): EvalOp[A, WithLabels[A]] = EngagementToLabel(e)
  }

  class LabelJudgementsBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, WithGroundTruth[A]] {
    override type G[a] = Int
    override type TC[a] = ResultLabels[A]
    override def create[E](threshold: Int)(implicit E: ResultLabels[A]): EvalOp[A, WithGroundTruth[A]] = BinaryRels(threshold)
  }
  class AtKBuilder[A] private[dsl] extends ArrowFactory[EvalOp, A, A] {
    override type G[a] = Rank
    override type TC[a] = AtK[A]
    override def create[E](k: Rank)(implicit E: AtK[A]): EvalOp[A, A] = EvalOp.K[A](k)
  }
  private[dsl] trait Syntax extends ExprSyntax0 with key.KeyBuilderSyntax {
    implicit def toRecOps[A](a: A): engagement.RecOps[A] = new engagement.RecOps(a)
    implicit def toJudgeOps[A](a: Judge[A]): dsl.judge.count.JudgementOps[A] = new dsl.judge.count.JudgementOps(a)
    implicit def toLabelerOps[A](a: Labeler[A]): dsl.label.count.LabelerOps[A] = new dsl.label.count.LabelerOps(a)
  }
  private[dsl] trait ExprSyntax0 {
    import engagement.LabelFor
    implicit def liftToLabelerOps[A](a: A)(implicit E: A LabelFor A): dsl.label.count.LabelerOps[A] = new dsl.label.count.LabelerOps(a.labeler)
  }
}