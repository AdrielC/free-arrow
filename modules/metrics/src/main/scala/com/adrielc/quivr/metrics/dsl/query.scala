package com.adrielc.quivr.metrics.dsl

import cats.Order
import matryoshka.data.Fix
import cats.implicits._
import com.adrielc.quivr.metrics.data.ResultSet

object query {

  type Query = Fix[QueryF]

  sealed trait QueryF[+A]
  object Query {
    case class Search(term: String)                         extends QueryF[Nothing]
    case class Taxonomy(id: Long, t: TaxLevel)              extends QueryF[Nothing]
    case class WithResults[A](query: A, ids: ResultSet)     extends QueryF[A]
  }

  sealed abstract class TaxLevel(val level: Int, val abbrev: String)
  object TaxLevel {
    case object Store       extends TaxLevel(1, "sto")
    case object Department  extends TaxLevel(2, "dep")
    case object Category    extends TaxLevel(3, "cat")
    case object Subcategory extends TaxLevel(4, "sub")

    implicit val orderTax: Order[TaxLevel] = Order.by(_.level)
  }
}
