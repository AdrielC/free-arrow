package com.adrielc.quivr.metrics.dsl

import cats.{Monoid, Order}
import com.adrielc.quivr.~>|
import cats.implicits._

case class DescribeOps[F[_, _], M: Monoid](
  describe: F ~>| M,
  order: Order[F[_, _]],
  prefix: M,
  delim: M,
  suffix: M) {

  def summarize(f: List[F[_, _]]): M =
    f.sorted(order.toOrdering)
      .map(describe(_))
      .foldSmash(prefix, delim, suffix)
}
