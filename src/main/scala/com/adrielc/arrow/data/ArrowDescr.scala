package com.adrielc.arrow.data

import cats.Monoid
import cats.arrow.ArrowChoice
import cats.implicits._
import io.circe.Json.fromJsonObject
import io.circe.{Json, JsonObject}


/**
 * Describes an Arrow going from [[A]] to [[B]] in a [[JsonObject]]. Useful for generating Json representations of arrow computation graphs
 */
case class ArrowDescr[-A, +B] private (obj: JsonObject) {

  def json: Json = Json.fromJsonObject(obj)

  def cast[C, D]: ArrowDescr[C, D] = this.asInstanceOf[ArrowDescr[C, D]]
}

object ArrowDescr {

  /**
   * Describe some operation that encodes an [[cats.arrow.Arrow]] from [[A]] to [[B]] in a Json value
   */
  def apply[A, B](describe: Json): ArrowDescr[A, B] = ArrowDescr(JsonObject("ops" -> Json.fromValues(List(describe))))

  def apply[A, B](string: String): ArrowDescr[A, B] = ArrowDescr(Json.fromString(string))

  implicit def monoidArrowDescr[A, B]: Monoid[ArrowDescr[A, B]] =
    Monoid.instance(ArrowDescr(JsonObject.empty), (a, b) => ArrowDescr(JsonObject.fromMap(a.obj.toMap |+| b.obj.toMap)))

  implicit val arrowChoiceArrowDescr: ArrowChoice[ArrowDescr] = new ArrowChoice[ArrowDescr] {

    override def lift[A, B](f: A => B): ArrowDescr[A, B] =
      ArrowDescr(JsonObject.empty)

    override def compose[A, B, C](f: ArrowDescr[B, C], g: ArrowDescr[A, B]): ArrowDescr[A, C] =
      f.cast[A, C] |+| g.cast[A, C]

    override def first[A, B, C](fa: ArrowDescr[A, B]): ArrowDescr[(A, C), (B, C)] =
      ArrowDescr(fa.obj)

    override def choose[A, B, C, D](f: ArrowDescr[A, C])(g: ArrowDescr[B, D]): ArrowDescr[Either[A, B], Either[C, D]] =
      ArrowDescr(JsonObject("choose" -> Json.obj("left" -> f.json, "right" -> g.json)))
  }


  /** similar to [[Json.deepMerge]], except it handles arrays via [[scala.collection.Iterator.++]] and numbers are added for any key collisions */
  implicit val monoidJson: Monoid[Json] = Monoid.instance(
    Json.fromJsonObject(JsonObject.empty),
    (a, b) =>
      (a.asArray, b.asArray) match {
        case (Some(v1), Some(v2)) => Json.fromValues(v2 ++ v1)
        case (Some(v), None) => Json.fromValues(v)
        case (None, Some(v)) => Json.fromValues(v)
        case _ =>

          (a.asObject, b.asObject) match {
            case (Some(lhs), Some(rhs)) =>
              fromJsonObject(
                lhs.toList.foldLeft(rhs) {
                  case (acc, (key, value)) =>
                    rhs(key).fold(acc.add(key, value)) { r => acc.add(key, value.deepMerge(r)) }
                }
              )

            case _ =>
              (a.asNumber, b.asNumber) match {
                case (Some(n1), Some(n2)) => Json.fromDoubleOrNull(n1.toDouble + n2.toDouble)
                case _ => b
              }
          }
      }
  )
}