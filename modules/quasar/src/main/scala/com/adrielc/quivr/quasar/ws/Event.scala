package com.adrielc.quivr.quasar.ws

import com.adrielc.quivr.quasar.Client
import io.circe.generic.auto._
import io.circe.{Encoder, Json}

trait Event[F[_]] {

  type A

  @transient val client: Client[F]
  @transient val encoder: Encoder[A]

  val d: A

  val op: Int
  val t: Option[String]

}

object Event {

  def encoder[F[_]]: Encoder[Event[F]] = (a: Event[F]) => {
    implicitly[Encoder[EventStruct]].apply(EventStruct(a.op, a.t, a.encoder(a.d)))
  }

  type Aux[F[_], E] = Event[F] { type A = E }

}

case class EventStruct(op: Int, t: Option[String], d: Json)
