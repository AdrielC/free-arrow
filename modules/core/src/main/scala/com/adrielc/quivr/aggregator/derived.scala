package com.adrielc.quivr.aggregator

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric}

object derived {

  trait DeriveAggregator extends MediumPriorityGenericAggregators {

    implicit def deriveLabelledGenericUnary[F, G <: HList]
    (implicit
     genA: LabelledGeneric.Aux[F, G],
     agg: Aggregator[G, G]
    ): Aggregator.Aux[F, F, agg.Agg] = agg.dimap(genA.to, genA.from)

    // deriving instances for fields that have field name specific aggregators in scope
    implicit def labelledAggHconsAggregator[K <: Symbol, HA, HB, TA <: HList, TB <: HList]
    (implicit
     lhA: FieldAggregator[HA, HB, K],
     tA: Aggregator[TA, TB]
    ): Aggregator.Aux[FieldType[K, HA] :: TA, FieldType[K, HB] :: TB, (lhA.ag.Agg, tA.Agg)] = lhA.ag splitHListTuple tA

    implicit val hNilAgg: Aggregator.Aux[HNil, HNil, String] = Const(HNil)
  }

  trait MediumPriorityGenericAggregators {

    implicit def labelledHconsAggregator[KA, KB, HA, HB, TA <: HList, TB <: HList]
    (implicit
     lhA: Aggregator[HA, HB],
     tA: Aggregator[TA, TB]
    ): Aggregator.Aux[FieldType[KA, HA] :: TA, FieldType[KB, HB] :: TB, (lhA.Agg, tA.Agg)] =
      lhA.map(field[KB](_)) splitHListTuple tA
  }
}