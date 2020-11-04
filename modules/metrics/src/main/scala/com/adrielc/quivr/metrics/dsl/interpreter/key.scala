package com.adrielc.quivr
package metrics
package dsl
package interpreter
import matryoshka.implicits._

object key {
  import dsl.evaluation.EvalOp
  import dsl.evaluation.EvalOp.MetricOp._
  import dsl.evaluation.EvalOp.EngagementOp._
  import dsl.evaluation.EvalOp._
  import dsl.engagement.{LabelerF, JudgeF}, LabelerF._, JudgeF._
  import dsl.key.SummarizeOps

  type MetricKeyBuilder   = SummarizeOps[EvalOp, String]

  def labelerToKey[E](e: engagement.Labeler[E]): String =
    e.cata[String] {
      case Count(e)         => s"${e.toString.toLowerCase}s"
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
      case Equiv(e, e1, e2) => s"(${labelerToKey(e1)}$e${labelerToKey(e2)})"
    }

  val defaultKeyBuilder: MetricKeyBuilder = SummarizeOps(
    new (EvalOp ~>| String) {
      def apply[A, B](fab: EvalOp[A, B]): String = fab match {
        case Ndcg(gain.pow2, _)       => "ndcg"
        case Ndcg(g, _)               => s"ndcg-$g"
        case Precision()              => "precision"
        case RPrecision()             => "r-precision"
        case Recall()                 => "recall"
        case FScore()                 => "f1"
        case AveragePrecision()       => "ap"
        case ReciprocalRank()         => "mrr"
        case b: BinaryRels[_]         => s"judgeLabel>${b.threshold}"
        case k: K[_]                  => s"@${k.k}"
        case EngagementToLabel(e)     => s"label(${labelerToKey(e)})"
        case EngagementToJudgement(e) => s"judge(${judgeToKey(e)})"
      }
    },
    new (EvalOp ~>| Int) {
      def apply[A, B](fab: EvalOp[A, B]): Int = fab match {
        case _: EngagementOp[_, _]  => 1
        case BinaryRels(_)          => 2
        case _: MetricOp[_, _]      => 3
        case K(_)                   => 4
      }
    },
    buildKeyGroups = "("-:",":-")",
    buildFullKey = "."
  )
}