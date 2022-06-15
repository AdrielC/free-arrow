package com.adrielc.quivr.quasar.ws.event

import com.adrielc.quivr.quasar.Client
import com.adrielc.quivr.quasar.ws.Event
import io.circe.Encoder

case class HeartbeatAck[F[_]](client: Client[F]) extends Event[F] {
  override type A = None.type
  override val encoder: Encoder[None.type] = implicitly[Encoder[None.type]]
  override val d: None.type = None
  override val op: Int = 11
  override val t: Option[String] = None
}
