package com.adrielc.arrow.data

import cats.Monoid
import cats.implicits._
import com.adrielc.arrow.ArrowChoicePlus
import io.circe.Json.fromJsonObject
import io.circe.{Json, JsonObject}


/**
 * Describes an Arrow going from [[A]] to [[B]] in a [[JsonObject]]. Useful for generating Json representations of arrow computation graphs
 */
case class JsonA[-A, +B] private(obj: JsonObject) {

  override def toString: String = json.spaces2

  def json: Json = Json.fromJsonObject(obj)

  def cast[C, D]: JsonA[C, D] = this.asInstanceOf[JsonA[C, D]]
}

object JsonA {

  /**
   * Describe some operation that encodes an [[cats.arrow.Arrow]] from [[A]] to [[B]] in a Json value
   */
  def apply[A, B](describe: Json): JsonA[A, B] = JsonA(JsonObject("ops" -> Json.fromValues(List(describe))))

  def apply[A, B](string: String): JsonA[A, B] = JsonA(Json.fromString(string))

  implicit def monoidArrowDescr[A, B]: Monoid[JsonA[A, B]] =
    Monoid.instance(JsonA(JsonObject.empty), (a, b) => JsonA(JsonObject.fromMap(a.obj.toMap |+| b.obj.toMap)))

  implicit val arrowDescrArrow: ArrowChoicePlus[JsonA] = new ArrowChoicePlus[JsonA] {

    override def zeroArrow[B, C]: JsonA[B, C] =
      JsonA(JsonObject.empty)

    override def plus[A, B](f: JsonA[A, B], g: JsonA[A, B]): JsonA[A, B] =
      JsonA(JsonObject("plus" -> Json.obj("f" -> f.json, "g" -> g.json)))

    override def lift[A, B](f: A => B): JsonA[A, B] =
      JsonA(JsonObject.empty)

    override def compose[A, B, C](f: JsonA[B, C], g: JsonA[A, B]): JsonA[A, C] =
      f.cast[A, C] |+| g.cast[A, C]

    override def first[A, B, C](fa: JsonA[A, B]): JsonA[(A, C), (B, C)] =
      JsonA(fa.obj)

    override def choose[A, B, C, D](f: JsonA[A, C])(g: JsonA[B, D]): JsonA[Either[A, B], Either[C, D]] =
      JsonA(JsonObject("choose" -> Json.obj("left" -> f.json, "right" -> g.json)))

    override def choice[A, B, C](f: JsonA[A, C], g: JsonA[B, C]): JsonA[Either[A, B], C] =
      JsonA(JsonObject("choice" -> Json.obj("left" -> f.json, "right" -> g.json)))

    override def left[A, B, C](fab: JsonA[A, B]): JsonA[Either[A, C], Either[B, C]] =
      JsonA(JsonObject("choose" -> Json.obj("left" -> fab.json)))

    override def right[A, B, C](fab: JsonA[A, B]): JsonA[Either[C, A], Either[C, B]] =
      JsonA(JsonObject("choose" -> Json.obj("right" -> fab.json)))
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