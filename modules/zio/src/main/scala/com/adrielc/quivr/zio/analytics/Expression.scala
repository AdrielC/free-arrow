package com.adrielc.quivr.zio.analytics

import zio.Chunk

case class Grouped[K, V](key: K, value: V)
case class Group[K, V](key: K, values: Chunk[V])
case class Timestamped[A](timestamp: Long, value: A)

sealed abstract class Expression[-A, +B] { self =>
  // We have to define `+` here rather than on the implicit classes
  // like everything else to shadow Predef's `any2stringadd`.
  def +[A1 <: A](that: A1 =>: Long)(implicit ev: B <:< Long): A1 =>: Long = {
    val _ = ev
    (self.asInstanceOf[A1 =>: Long] &&& that) >>> Expression.Sum
  }
}

object Expression {

  case class Id[A]()                                                           extends Expression[A, A]
  case class Compose[A, B, C](f: Expression[B, C], g: Expression[A, B])        extends Expression[A, C]
  case class FanOut[A, B, C](f: Expression[A, B], g: Expression[A, C])         extends Expression[A, (B, C)]
  case class Split[A, B, C, D](f: Expression[A, C], g: Expression[B, D])       extends Expression[(A, B), (C, D)]
  case class FlipTuple[A, B]()                                                 extends Expression[(A, B), (B, A)]

  case class LongLiteral[A](l: Long)                                           extends Expression[A, Long]
  case class StringLiteral[A](s: String)                                       extends Expression[A, String]
  case class BooleanLiteral[A](b: Boolean)                                     extends Expression[A, Boolean]
  case class KeyValue[A, K, V](key: Expression[A, K], value: Expression[A, V]) extends Expression[A, Grouped[K, V]]
  case class Length[V]()                                                       extends Expression[List[V], Long]
  case class GroupKey[K, V]()                                                  extends Expression[Group[K, V], K]
  case class GroupValues[K, V]()                                               extends Expression[Group[K, V], List[V]]
  case class GroupedKey[K, V]()                                                extends Expression[Grouped[K, V], K]
  case class GroupedValue[K, V]()                                              extends Expression[Grouped[K, V], V]
  case class ConstructGrouped[K, V]()                                          extends Expression[(K, V), Grouped[K, V]]
  case object ListSum                                                          extends Expression[List[Long], Long]

  case object Mul   extends Expression[(Long, Long), Long]
  case object Sum   extends Expression[(Long, Long), Long]
  case object Split extends Expression[(String, String), List[String]]

  case class NthColumn[A <: Product, B](n: Int) extends Expression[A, B]

  case class TimestampedTimestamp[A]() extends Expression[Timestamped[A], Long]
  case class TimestampedValue[A]()     extends Expression[Timestamped[A], A]

  implicit class FunctionOps[A, B](f: A =>: B) {
    def >>>[C](g: B =>: C): A =>: C              = Compose(g, f)
    def <<<[C](g: C =>: A): C =>: B              = Compose(f, g)
    def &&&[C](g: A =>: C): A =>: (B, C)         = FanOut(f, g)
    def ***[C, D](g: C =>: D): (A, C) =>: (B, D) = Split(f, g)
  }

  implicit class NumberOps[A](x: A =>: Long) {
    def *(y: A =>: Long): A =>: Long = (x &&& y) >>> Mul
  }

  implicit class StringOps[A](s: A =>: String) {
    def split(delimiter: A =>: String): A =>: List[String] = (s &&& delimiter) >>> Split
  }

  implicit class Tuple2Ops[A, B, C](tp: A =>: (B, C)) {
    def _1: A =>: B = tp >>> NthColumn(0)
    def _2: A =>: C = tp >>> NthColumn(1)
  }

  implicit class Tuple3Ops[A, B, C, D](tp: A =>: (B, C, D)) {
    def _1: A =>: B = tp >>> NthColumn(0)
    def _2: A =>: C = tp >>> NthColumn(1)
    def _3: A =>: D = tp >>> NthColumn(2)
  }

  implicit class ListOps[A, B](l: A =>: List[B]) {
    def length: A =>: Long = l >>> Length[B]
  }

  implicit class LongListOps[A](l: A =>: List[Long]) {
    def sum: A =>: Long = l >>> ListSum
  }

  implicit class GroupOps[A, K, V](g: A =>: Group[K, V]) {
    def key: A =>: K          = g >>> GroupKey[K, V]
    def values: A =>: List[V] = g >>> GroupValues[K, V]
  }

  implicit class GroupedOps[A, K, V](g: A =>: Grouped[K, V]) {
    def key: A =>: K    = g >>> GroupedKey[K, V]
    def values: A =>: V = g >>> GroupedValue[K, V]
  }

  implicit class TimestampedOps[A, V](t: A =>: Timestamped[V]) {
    def timestamp: A =>: Long = t >>> TimestampedTimestamp()
    def value: A =>: V        = t >>> TimestampedValue()
  }

  implicit def liftSingle[A, B](b: B)(implicit B: Type[B]): Expression[A, B]            = B.lift(b)
  implicit def sequenceTuple[A, B, C](tp: (A =>: B, A =>: C)): A =>: (B, C)             = FanOut(tp._1, tp._2)
  implicit def liftMixedL[A, B, C](tp: (B, A =>: C))(implicit B: Type[B]): A =>: (B, C) = FanOut(B.lift(tp._1), tp._2)
  implicit def liftMixedR[A, B, C](tp: (A =>: B, C))(implicit C: Type[C]): A =>: (B, C) = FanOut(tp._1, C.lift(tp._2))
}
