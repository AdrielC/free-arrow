package com.adrielc.quivr.metrics.api

import com.adrielc.quivr.metrics.ResultRels
import com.adrielc.quivr.metrics.data.EngagedResults
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.{+>, EngRes}
import shapeless.ops.hlist.LeftReducer
import shapeless.{=:!=, Generic, HList, Lazy, Poly2}

trait ToRel[A] extends Serializable {
  type E
  def apply(a: A): EngagedResults[Map[E, Int]] +> ResultRels
}
object ToRel {
  type Aux[A, Eng] = ToRel[A] { type E = Eng }
  def instance[A, Eng](f: A => EngRes[Eng] +> ResultRels): ToRel.Aux[A, Eng] = new ToRel[A] {
    override type E = Eng
    override def apply(a: A): EngRes[Eng] +> ResultRels = f(a)
  }
  implicit def fromLabeler[Eng]: ToRel.Aux[Labeler[Eng], Eng] = instance(_.liftA)
  implicit def fromJudge[Eng]: ToRel.Aux[Judge[Eng], Eng] = instance(_.liftA)
  implicit def hListGenReducer[H, G](implicit G: Generic.Aux[H, G], E: Lazy[ToRel[G]]): ToRel.Aux[H, E.value.E] = instance(a => E.value(G.to(a)))

  implicit def fromTupleOfJudgeOrLabel[H <: HList, Eng]
  (implicit M: Lazy[LeftReducer.Aux[H, labelBuilder.type, EngRes[Eng] +> ResultRels]]): ToRel.Aux[H, Eng] = instance(M.value.apply)

  object labelBuilder extends InjectLabel {
    implicit def labelFromSameValues[A, B, E]
    (implicit LA: Lazy[ToRel.Aux[A, E]], LB: Lazy[ToRel.Aux[B, E]]): Case.Aux[A, B, EngRes[E] +> ResultRels] = at[A, B]((a, b) => LA.value(a) <+> LB.value(b))
  }
  trait InjectLabel extends Poly2 {
    implicit def labelerLabel[A, B, E1, E2]
    (implicit LA: Lazy[ToRel.Aux[A, E1]], LB: Lazy[ToRel.Aux[B, E2]], ev: E1 =:!= E2): Case.Aux[A, B, EngRes[Either[E1, E2]] +> ResultRels] = {
      val _ = ev
      at[A, B]((a, b) =>
        LA.value(a).lmap { (r: EngRes[Either[E1, E2]]) =>
          r.copy(engagements = r.engagements.map(_.collect { case (Left(e), c) => e -> c }))
        } <+> LB.value(b).lmap { (r: EngRes[Either[E1, E2]]) =>
          r.copy(engagements = r.engagements.map(_.collect { case (Right(e), c) => e -> c }))
        }
      )
    }
  }
}
