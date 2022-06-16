package com.adrielc.quivr.metrics
package dsl
package interpreter
import com.adrielc.quivr.~>|
import matryoshka.implicits._

object key {
  import function.{double, gain}
  import dsl.evaluation.EvalOp
  import dsl.evaluation.EvalOp._
  import dsl.engagement.{JudgeF, LabelerF}
  import LabelerF._
  import JudgeF._
  import dsl.key._

  type MetricKeyBuilder   = SummarizeOps[EvalOp, String]

  private def formatDouble(d: Double): String =
    if (d.isValidInt) d.toInt.toString else d.toString.replace('.', 'p')

  private val labelerToKey: engagement.Labeler[_] => String = _.cata[String] {
    case Count(e)         => s"${e.toString.toLowerCase}"
    case Value(d)         => formatDouble(d)
    case Mult(e1, e2)     => s"${e1}_*_$e2"
    case Sum(e1, e2)      => s"${e1}_+_$e2"
    case Log(e1, base)    => s"log_${formatDouble(base)}($e1)"
    case Div(e1, e2)      => s"${e1}_/_$e2"
    case As(i, t)         => s"${judgeToKey(i)}_?_$t"
    case Or(e1, e2)       => s"${e1}_|_$e2"
    case And(e1, e2)      => s"${e1}_&_$e2"
    case Equiv(e, e1, e2) => s"$e1$e$e2"
  }

  private val judgeToKey: engagement.Judge[_] => String = _.cata[String] {
    case Or(e1, e2)             => s"(${e1}_|_$e2)"
    case And(e1, e2)            => s"(${e1}_&_$e2)"
    case Equiv(e, e1, e2)       => (e, e2) match {
      case (double.>, "0") => s"bin${(e1).toLowerCase()})" // handle special binary case
      case _ => s"($e1$e$e2)"
    }
  }

  val defaultKeyBuilder: MetricKeyBuilder = SummarizeOps(
    new (EvalOp ~>| String) {
      import MetricOp._
      def apply[A, B](fab: EvalOp[A, B]): String = fab match {
        case Ndcg(gain.pow2, _)         => "ndcg"
        case QMeasure(1)                => "q"
        case Ndcg(g, _)                 => s"ndcg-G$g"
        case QMeasure(b)                => s"q-B${formatDouble(b)}"
        case Precision()                => "prec"
        case RPrecision()               => "rPrec"
        case Recall()                   => "recall"
        case FScore()                   => "f1"
        case AveragePrecision()         => "ap"
        case ReciprocalRank()           => "mrr"
        case EngagementToLabel(e)       => labelerToKey(e)
        case EngagementToJudgement(e)   => judgeToKey(e)
        case LabelsToJudgement(eq, d)   => s"judge($eq$d)"
        case ToEngagedResults()         => ""
        case k: K[_]                    => s"@${k.k}"
      }
    },
    new (EvalOp ~>| Int) {
      def apply[A, B](fab: EvalOp[A, B]): Int = fab match {
        case _: EngagementOp[_, _]  => 1
        case _: MetricOp[_]         => 2
        case K(_)                   => 3
      }
    },
    buildKeyGroups = "["-:",":-"]",
    buildFullKey = "."
  )
}
