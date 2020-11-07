package com.adrielc.quivr.metrics
package dsl

import java.time.Instant

import cats.data.{NonEmptyList, NonEmptyMap}

object query {

  sealed trait QueryOp[A, B]
  object QueryOp {
    final case class QueryResults(system: System)                  extends QueryOp[Query, NonEmptyList[ResultId]]
    final case class QueryGroundTruth(from: Instant, to: Instant)  extends QueryOp[Query, NonEmptyMap[ResultId, Label]]
  }

  sealed trait System
  case object Control                     extends System
  final case class Variant(name: String)  extends System


  sealed trait Query
  case class Search(keyword: String)
  case class Navigation(taxonomy: String)
}

