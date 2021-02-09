package com.adrielc.quivr.metrics.api

import com.adrielc.quivr.metrics.ResultRels
import com.adrielc.quivr.metrics.dsl.engagement.{Judge, Labeler}
import com.adrielc.quivr.metrics.dsl.+>
import shapeless.ops.hlist.LeftReducer
import shapeless.{=:!=, Generic, HList, Lazy, Poly2}

trait ToRel[A] extends Serializable {
  type E
  def toRel(a: A): Res[E] +> ResultRels
}
object ToRel {
  type Aux[A, Eng] = ToRel[A] { type E = Eng }


  def instance[A, Eng](f: A => Res[Eng] +> ResultRels): ToRel.Aux[A, Eng] = new ToRel[A] {
    override type E = Eng
    override def toRel(a: A): Res[Eng] +> ResultRels = f(a)
  }

  implicit def reflexive[Eng]: ToRel.Aux[Res[Eng] +> ResultRels, Eng] =
    instance(identity)

  implicit def fromLabeler[Eng]: ToRel.Aux[Labeler[Eng], Eng] =
    instance(_.from[Res[Eng]])

  implicit def fromJudge[Eng]: ToRel.Aux[Judge[Eng], Eng] =
    instance(_.from[Res[Eng]])

  implicit def hListGenReducer[H, G]
  (implicit G: Generic.Aux[H, G], E: Lazy[ToRel[G]]): ToRel.Aux[H, E.value.E] =
    instance(a => E.value.toRel(G.to(a)))

  implicit def fromTupleOfJudgeOrLabel[H <: HList, Eng]
  (implicit M: Lazy[LeftReducer.Aux[H, labelBuilder.type, Res[Eng] +> ResultRels]]): ToRel.Aux[H, Eng] =
    instance(M.value.apply)

  object labelBuilder extends InjectLabel {

    implicit def labelFromSameValues[A, B, E]
    (implicit LA: Lazy[ToRel.Aux[A, E]], LB: Lazy[ToRel.Aux[B, E]]): Case.Aux[A, B, Res[E] +> ResultRels] =
      at[A, B]((a, b) => LA.value.toRel(a) <+> LB.value.toRel(b))
  }
  trait InjectLabel extends Poly2 {
    implicit def labelerLabel[A, B, E1, E2]
    (implicit LA: Lazy[ToRel.Aux[A, E1]], LB: Lazy[ToRel.Aux[B, E2]], ev: E1 =:!= E2): Case.Aux[A, B, Res[Either[E1, E2]] +> ResultRels] = {
      val _ = ev
      at[A, B]((a, b) =>
        LA.value.toRel(a).lmap { (r: Res[Either[E1, E2]]) =>
          r.copy(_2 = r._2.mapValues(_.collect { case (Left(e), c) => e -> c }))
        } <+> LB.value.toRel(b).lmap { (r: Res[Either[E1, E2]]) =>
          r.copy(_2 = r._2.mapValues(_.collect { case (Right(e), c) => e -> c }))
        }
      )
    }
  }
}