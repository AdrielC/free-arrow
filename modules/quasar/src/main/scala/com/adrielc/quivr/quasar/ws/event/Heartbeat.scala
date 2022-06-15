package com.adrielc.quivr.quasar.ws.event

import com.adrielc.quivr.quasar.Client
import com.adrielc.quivr.quasar.ws.Event
import io.circe.syntax._
import io.circe.{Encoder, Json}
import spire.math.ULong

case class Heartbeat[F[_]](client: Client[F], d: Option[ULong]) extends Event[F] {

  override type A = Option[ULong]
  override val encoder: Encoder[Option[ULong]] = (u: Option[ULong]) => u.fold(Json.Null)(_.toBigInt.asJson)

  override val op: Int = 1
  override val t: Option[String] = None

}
