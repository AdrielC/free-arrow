package com.adrielc.quivr.metrics.dsl

import cats.data.NonEmptyList
import com.adrielc.quivr.metrics.dsl.engagement.ExprF
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.EngagementOp.{EngagementToLabel, EngagementToJudgement}
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.MetricOp._
import com.adrielc.quivr.metrics.dsl.evaluation.EvalOp.{BinaryRelevance, EngagementOp, K, MetricOp}
import com.adrielc.quivr.metrics.function.Gain.Pow2
import com.adrielc.quivr.~>|
import cats.implicits._
import matryoshka.Recursive
import matryoshka.implicits._

object serialize {

  type MetricKeyBuilder   = SummarizeOps[EvalOp]

  case class SummarizeOps[F[_, _]](
    describe: F ~>| String,
    order   : F ~>| Int,
    prefix  : String = "",
    delim   : String = ".",
    suffix  : String = ""
  ) {
    def summarize(f: List[F[_, _]]): String =
      f.groupByNel(order(_))
        .map(_._2 match {
          case NonEmptyList(h, Nil) => describe(h)
          case n @ NonEmptyList(_, _) => n.map(describe(_)).foldSmash("(", ",", ")")
        })
        .toList
        .foldSmash(prefix, delim, suffix)
  }

  def evalToString[T](e: T)(implicit T: Recursive.Aux[T, ExprF[Any, *]]): String = e.cata[String] {
    case ExprF.Count(e)             => s"${e.toString.toLowerCase}s"
    case ExprF.Value(value)         => value.toString.replace('.', 'p')
    case ExprF.Sign(e)              => s"sign($e)"
    case ExprF.Binary(exp)          => s"binary($exp)"
    case ExprF.Mult(e1, e2)         => s"($e1*$e2)"
    case ExprF.Sum(e1, e2)          => s"($e1+$e2)"
    case ExprF.IfThen(i, t)         => s"if($i,$t)"
    case ExprF.Div(e1, e2)          => s"($e1/$e2)"
    case ExprF.Or(e1, e2)           => s"($e1|$e2)"
    case ExprF.And(e1, e2)          => s"($e1&$e2)"
    case ExprF.Eq(e, e1, e2)        => s"($e1$e$e2)"
  }


  val defaultKeys: MetricKeyBuilder = SummarizeOps(
    new (EvalOp ~>| String) {
      def apply[A, B](fab: EvalOp[A, B]): String = fab match {
        case Ndcg(Pow2)               => "ndcg"
        case Ndcg(g)                  => s"ndcg-$g"
        case Precision()              => "precision"
        case RPrecision()             => "r-precision"
        case Recall()                 => "recall"
        case FScore()                 => "f1"
        case AveragePrecision()       => "ap"
        case ReciprocalRank()         => "mrr"
        case EngagementToLabel(e)     => s"label(${evalToString(e)})"
        case EngagementToJudgement(e) => s"judge(${evalToString(e)})"
        case b: BinaryRelevance[_]    => s"judgeLabel>${b.threshold}"
        case k: K[_]                  => s"@${k.k}"
      }
    },
    new (EvalOp ~>| Int) {
      def apply[A, B](fab: EvalOp[A, B]): Int = fab match {
        case _: EngagementOp[_, _]  => 1
        case BinaryRelevance(_)     => 2
        case _: MetricOp[_, _]      => 3
        case K(_)                   => 4
      }
    }
  )
}
