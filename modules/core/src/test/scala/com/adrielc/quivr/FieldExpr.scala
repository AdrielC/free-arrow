package com.adrielc.quivr

import cats.arrow.{Arrow, ArrowChoice}
import com.adrielc.quivr.data.~~>
import com.adrielc.quivr.free.FreeA
import com.adrielc.quivr.free.FreeA.liftK
import cats.implicits._

sealed trait FieldExpr[A, B]

object FieldExpr {

  type Json = Map[String, String]

  case class IsNotNull(field: String)   extends FieldExpr[Json, Either[Json, Json]]
  case class IsTrue(field: String)      extends FieldExpr[Json, Either[Json, Json]]

  sealed trait OCode                    extends FieldExpr[Json, OCode]
  case object ExistingOCode             extends OCode
  case object NewOCode                  extends OCode


  type FieldOp[O] = FreeA[Arrow, FieldExpr, Json, O]

  def isNotNull(field: String): FieldOp[Either[Json, Json]] = liftK(IsNotNull(field))
  def isTrue(field: String)   : FieldOp[Either[Json, Json]] = liftK(IsTrue(field))
  val existingOcode           : FieldOp[OCode]              = liftK(ExistingOCode)
  val newOcode                : FieldOp[OCode]              = liftK(NewOCode)

}


object FieldExprApp extends App {
  import FieldExpr._

  val newOrExisting = newOcode ||| existingOcode

  val flow: FreeA[ArrowChoice, FieldExpr, Json, OCode] =
    isNotNull("customerId") >>>
      ((isTrue("customerid_is_ocode") >>> newOrExisting) |||
        (isNotNull("ehid") >>> (newOcode ||| (isTrue("ehid_is_ocode") >>> newOrExisting))))


  val getCode = flow.foldMap(new (FieldExpr ~~> Function1) {
    def apply[A, B](fab: FieldExpr[A, B]): A => B = fab match {
      case IsNotNull(field) => (j: Json) => (if(j.get(field).fold(false)(_ != "null")) Right[Json, Json](j) else Left[Json, Json](j)): Either[Json, Json]
      case IsTrue(field) => (j: Json) => (if(j.get(field).fold(false)(_ == "true")) Right(j) else Left(j)): Either[Json, Json]
      case code: OCode => _ => code
    }
  })


  val code = getCode(Map(
    "customerId"          -> "code",
    "customerid_is_ocode" -> "false"
  ))

  println(code)
}
