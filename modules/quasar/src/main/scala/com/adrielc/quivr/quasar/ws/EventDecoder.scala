package com.adrielc.quivr.quasar.ws

import com.adrielc.quivr.quasar.Client
import io.circe.DecodingFailure

trait EventDecoder {

  def decode[F[_]](client: Client[F]): PartialFunction[EventStruct, Either[DecodingFailure, Event[F]]]

}
