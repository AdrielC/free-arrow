package com.adrielc.quivr.quasar
package ws.event

import com.adrielc.quivr.quasar.ws.Event
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.semiauto._

case class Hello[F[_]](client: Client[F], d: HelloData) extends Event[F] {

  override type A = HelloData
  override val encoder: Encoder[HelloData] = HelloData.helloDataEncoder

  override val op: Int = 10
  override val t: Option[String] = None

}

case class HelloData(heartbeatInterval: Int)

object HelloData {

  implicit val helloDataEncoder: Encoder[HelloData] = deriveConfiguredEncoder[HelloData]
  implicit val helloDataDecoder: Decoder[HelloData] = deriveConfiguredDecoder[HelloData]
}
