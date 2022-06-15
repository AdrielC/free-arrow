package com.adrielc.quivr.quasar

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, Sync, Timer}
import cats.implicits._
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import io.circe.DecodingFailure
import java.util.concurrent.TimeUnit

import com.adrielc.quivr.quasar.model.MessageLike
import com.adrielc.quivr.quasar.ws.event._
import com.adrielc.quivr.quasar.ws.{Event, EventDecoder, EventStruct, Socket}
import org.http4s.client.jdkhttpclient._
import org.http4s.client.{Client => HttpClient}

import scala.concurrent.duration.FiniteDuration
import spire.math.ULong

object Defaults {

  def httpClient[F[_]: ConcurrentEffect: ContextShift: Timer]: F[(HttpClient[F], WSClient[F])] =
    Sync[F].delay(java.net.http.HttpClient.newHttpClient()).map { jdkHttpClient =>
      (JdkHttpClient(jdkHttpClient), JdkWSClient(jdkHttpClient))
    }

  def socketDeferred[F[_]: Concurrent]: F[Deferred[F, Socket[F]]] = Deferred[F, Socket[F]]

  def eventTopic[F[_]: Concurrent](c: Client[F]): F[Topic[F, Event[F]]] = Topic(HeartbeatAck(c))

  def eventQueue[F[_]: Concurrent]: F[Queue[F, Event[F]]] = Queue.unbounded[F, Event[F]]

  def sequenceRef[F[_]: Sync]: F[Ref[F, Option[ULong]]] = Ref.of[F, Option[ULong]](None)

  def defaultEventHandler[F[_]: Timer: Concurrent]: EventHandler[F] =
    _ =>
      _.flatMap {
        case Hello(cli, HelloData(interval)) =>
          val beat: Stream[F, Unit] = for {
            _ <- Stream[F, FiniteDuration](FiniteDuration.apply(0, TimeUnit.MILLISECONDS)) ++ Stream.awakeDelay[F](
              FiniteDuration(interval.toLong, TimeUnit.MILLISECONDS)
            )
            sock <- Stream.eval(cli.deferredSocket.get)
            seq <- Stream.eval(sock.sequence.get)
            heartbeat = Heartbeat(cli, seq)
            sent <- Stream.eval(sock.send(heartbeat))
          } yield sent
          val identify: Stream[F, Unit] = for {
            sock <- Stream.eval(cli.deferredSocket.get)
            identify = Identify(cli, IdentifyData(cli.token))
            sent <- Stream.eval(sock.send(identify))
          } yield sent
          identify ++ beat
        case _ => Stream.empty
    }

  val defaultEventDecoder: EventDecoder = new EventDecoder {
    override def decode[F[_]](client: Client[F]): PartialFunction[EventStruct, Either[DecodingFailure, Event[F]]] = {
      case EventStruct(0, Some("READY"), d) =>
        d.as[ReadyData].map(Ready(client, _))
      case EventStruct(0, Some("MESSAGE_CREATE"), d) =>
        d.as[MessageLike].map(MessageCreate(client, _))
      case EventStruct(10, None, d) =>
        d.as[HelloData].map(Hello(client, _))
      case EventStruct(11, None, _) =>
        Right(HeartbeatAck(client))
    }
  }

}
