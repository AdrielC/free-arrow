package com.adrielc.quivr.quasar
package ws.event

import com.adrielc.quivr.quasar.ws.Event
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.extras.semiauto._

case class Ready[F[_]](client: Client[F], d: ReadyData) extends Event[F] {

  override type A = ReadyData
  override val encoder: Encoder[ReadyData] = ReadyData.readyDataEncoder

  override val op: Int = 0
  override val t: Option[String] = Some("READY")
}

case class ReadyData(
  v: Int,
  user: Json,
  guilds: List[Json], // TODO
  sessionId: String
)

object ReadyData {

  implicit val readyDataEncoder: Encoder[ReadyData] = deriveConfiguredEncoder
  implicit val readyDataDecoder: Decoder[ReadyData] = deriveConfiguredDecoder

}
