package com.adrielc.quivr
package metrics
package dsl
package interpreter
import matryoshka.implicits._

object key {
  import dsl.evaluation.EvalOp
  import dsl.evaluation.EvalOp._
  import dsl.engagement.{LabelerF, JudgeF}, LabelerF._, JudgeF._
  import dsl.key._
  import function.gain

  type MetricKeyBuilder   = SummarizeOps[EvalOp, String]

  def labelerToKey[E](e: engagement.Labeler[E]): String =
    e.cata[String] {
      case Count(e)         => s"${e.toString.toLowerCase}"
      case Value(d)         => if (d.isValidInt) d.toInt.toString else e.toString.replace('.', 'p')
      case Mult(e1, e2)     => s"($e1*$e2)"
      case Sum(e1, e2)      => s"($e1+$e2)"
      case Div(e1, e2)      => s"($e1/$e2)"
      case IfThen(i, t)     => s"if(${judgeToKey(i)},$t)"
      case Or(e1, e2)       => s"($e1|$e2)"
      case And(e1, e2)      => s"($e1&$e2)"
      case Equiv(e, e1, e2) => s"filter(${labelerToKey(e1)}$e${labelerToKey(e2)}"
    }

  def judgeToKey[E](e: engagement.Judge[E]): String =
    e.cata[String] {
      case Or(e1, e2)       => s"($e1|$e2)"
      case And(e1, e2)      => s"($e1&$e2)"
      case Equiv(e, e1, e2) => (e, e2.unFix) match {
        case (function.double.>, Value(0)) => s"binary${labelerToKey(e1).capitalize}" // handle special binary case
        case _ => s"(${labelerToKey(e1)}$e${labelerToKey(e2)})"
      }
    }

  val defaultKeyBuilder: MetricKeyBuilder = SummarizeOps(
    new (EvalOp ~>| String) {
      def apply[A, B](fab: EvalOp[A, B]): String = fab match {
        case ResultCountEq(eq, k) => s"filterK$eq$k"
        case Ndcg(gain.pow2, _) => "ndcg"
        case QMeasure(b) => s"q-B$b"
        case Ndcg(g, _) => s"ndcg-G$g"
        case Precision() => "prec"
        case RPrecision() => "rPrec"
        case Recall() => "recall"
        case FScore() => "f1"
        case AveragePrecision() => "ap"
        case ReciprocalRank() => "mrr"
        case EngagementToLabel(e) => labelerToKey(e)
        case EngagementToJudgement(e) => judgeToKey(e)
        case k: K[_] => s"@${k.k}"
      }
    },
    new (EvalOp ~>| Int) {
      def apply[A, B](fab: EvalOp[A, B]): Int = fab match {
        case _: FilterOp[_]         => 1
        case _: EngagementOp[_, _]  => 2
        case _: MetricOp[_, _]      => 3
        case K(_)                   => 4
      }
    },
    buildKeyGroups = "("-:",":-")",
    buildFullKey = "."
  )
}
