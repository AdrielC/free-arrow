package com.adrielc.quivr.quasar.ws.event

import com.adrielc.quivr.quasar.Client
import com.adrielc.quivr.quasar.model.MessageLike
import com.adrielc.quivr.quasar.ws.Event
import io.circe.Encoder

case class MessageCreate[F[_]](client: Client[F], d: MessageLike) extends Event[F] {

  override type A = MessageLike
  override val encoder: Encoder[MessageLike] = Encoder.instance(_ => ???)

  override val op: Int = 0
  override val t: Option[String] = Some("MESSAGE_CREATE")
}
