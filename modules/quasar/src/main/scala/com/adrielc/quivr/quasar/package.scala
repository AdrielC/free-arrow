package com.adrielc.quivr

import cats.effect.Ref
import fs2.{io => _, _}
import io.circe.generic.extras._
import io.circe.{Decoder, DecodingFailure, HCursor}
import java.time.ZonedDateTime

import com.adrielc.quivr.quasar.ws.Event
import spire.math.ULong

import scala.util.Try

package object quasar {

  type EventHandler[F[_]] = Ref[F, Option[ULong]] => Pipe[F, Event[F], Unit]
  type EventHandlers[F[_]] = List[EventHandler[F]]

  object EventHandler {

    def apply[F[_]](f: PartialFunction[Event[F], Stream[F, Unit]]): EventHandler[F] = _ => _.collect(f).flatten

  }

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults

  implicit val ulongDecoder: Decoder[ULong] = (c: HCursor) => {
    val strO = c.value.asString
    strO.flatMap(s => Try(ULong(s)).toOption)
      .toRight(DecodingFailure(s"Attempt to decode non-integer as ULong: ${c.value}", List()))
  }

  implicit val timestampDecoder: Decoder[ZonedDateTime] = (c: HCursor) => {
    val strO = c.value.asString
    strO
      .map(ZonedDateTime.parse)
      .toRight(DecodingFailure(s"Attempt to decode non-timestamp as ZonedDateTime: ${c.value}", List()))
  }

  implicit def listDecoder[A: Decoder]: Decoder[List[A]] =
    Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(List.empty))

}
