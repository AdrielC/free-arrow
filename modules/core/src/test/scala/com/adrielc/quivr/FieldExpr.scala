package com.adrielc.quivr

import cats.arrow.Arrow
import com.adrielc.quivr.data.~~>
import com.adrielc.quivr.free.FreeArrow
import com.adrielc.quivr.free.FreeArrow.liftK
import cats.instances.all._
import org.scalatest.{FlatSpec, Matchers}

sealed trait FieldExpr[A, B]

object FieldExpr {

  type Json = Map[String, String]

  case class IsNotNull(field: String)   extends FieldExpr[Json, Boolean]
  case class IsTrue(field: String)      extends FieldExpr[Json, Boolean]

  sealed trait OCode                    extends FieldExpr[Json, OCode]
  case object ExistingOCode             extends OCode
  case object NewOCode                  extends OCode


  type FieldOp[O] = FreeArrow[Arrow, FieldExpr, Json, O]

  def isNotNull(field: String): FieldOp[Boolean] = liftK(IsNotNull(field))
  def isTrue(field: String)   : FieldOp[Boolean] = liftK(IsTrue(field))
  val existingOcode           : FieldOp[OCode]   = liftK(ExistingOCode)
  val newOcode                : FieldOp[OCode]   = liftK(NewOCode)

}


class FieldExprApp extends FlatSpec with Matchers {
  import FieldExpr._

  val newOrExisting = newOcode ||| existingOcode

  val flow =
    isNotNull("customerId").test >>>
      ((isTrue("customerid_is_ocode").test >>> newOrExisting) |||
        (isNotNull("ehid").test >>> (newOcode ||| (isTrue("ehid_is_ocode").test >>> newOrExisting))))


  val getCode = flow.foldMap(new (FieldExpr ~~> Function1) {
    def apply[A, B](fab: FieldExpr[A, B]): A => B = fab match {
      case IsNotNull(field) => (j: Json) => j.get(field).contains("null")
      case IsTrue(field)    => (j: Json) => j.get(field).contains("true")
      case code: OCode      => _ => code
    }
  })


  "FieldExpr" should "evaluate" in {

    val code = getCode(Map(
      "customerId"          -> "code",
      "customerid_is_ocode" -> "false"
    ))

    assert(code == NewOCode)
  }
}
