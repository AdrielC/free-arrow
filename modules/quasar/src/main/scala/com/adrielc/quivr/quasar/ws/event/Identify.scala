package com.adrielc.quivr.quasar
package ws.event

import com.adrielc.quivr.quasar.ws.Event
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.JsonKey
import io.circe.generic.extras.semiauto._

case class Identify[F[_]](client: Client[F], d: IdentifyData) extends Event[F] {

  override type A = IdentifyData
  override val encoder: Encoder[IdentifyData] = IdentifyData.identifyDataEncoder

  override val op: Int = 2
  override val t: Option[String] = None

}

case class IdentifyData(token: String, properties: IdentifyProperties = IdentifyProperties())

object IdentifyData {

  implicit val identifyDataEncoder: Encoder[IdentifyData] = deriveConfiguredEncoder
  implicit val identifyDataDecoder: Decoder[IdentifyData] = deriveConfiguredDecoder

}

case class IdentifyProperties(
  @JsonKey(s"$$os") os: String = "linux",
  @JsonKey(s"$$browser") browser: String = "discocat",
  @JsonKey(s"$$device") device: String = "discocat"
)

object IdentifyProperties {

  implicit val identifyPropertiesEncoder: Encoder[IdentifyProperties] = deriveConfiguredEncoder
  implicit val identifyPropertiesDecoder: Decoder[IdentifyProperties] = deriveConfiguredDecoder

}
