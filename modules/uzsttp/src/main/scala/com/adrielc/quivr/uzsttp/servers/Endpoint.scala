package com.adrielc.quivr.uzsttp.servers

import uzhttp.Request.{Method, WebsocketRequest}
import uzhttp.{HTTPError, Request, Response}
import zio._
import EndPoint.HRequest
import cats.implicits._
import cats.data.NonEmptyList
import uzhttp.HTTPError.{BadRequest, NotFound}
import zio.stream._
import hrequest._
import uzhttp.websocket._


object EndPoint {
  type HRequest = Has[Request]

  type EndPoint[R <: HRequest] = ZIO[R, Option[HTTPError], Response]

  def endsWith(ss: NonEmptyList[String])(in: Seq[String]): Boolean =
    in.endsWith(ss.toList)

  def endsWith(s: String)(in: Seq[String]): Boolean =
    in.endsWith(List(s))

  def startsWith(ss: NonEmptyList[String])(in: Seq[String]): Boolean =
    in.startsWith(ss.toList)

  def startsWith(s: String)(in: Seq[String]): Boolean = {
    println(s"compere $s to : $in")
    in.startsWith(List(s))
  }

  def uriMethod(pMatch: Seq[String] => Boolean, expectedMethod: Method): ZIO[HRequest, Option[HTTPError], Unit] = {
    for {
      pth <- uri
      mtd <- method
      matched <- if (pMatch(pth) && (mtd == expectedMethod))
        IO.unit else IO.fail(None)
    } yield matched
  }

  def orNotFound[R <: HRequest](p: EndPoint[R]): ZIO[R, HTTPError, Response] =
    for {
      r <- request
      pp <- p.mapError {
        case Some(err) => err
        case None => NotFound(r.uri.getPath)
      }
    } yield pp

  def noAuthHandler(p: EndPoint[HRequest]): Request => IO[HTTPError, Response] =
  { req: Request =>
    orNotFound(p).provideLayer(ZLayer.succeed(req))
  }

  def combineRoutes[R <: HRequest](h: EndPoint[R], t: EndPoint[R]*): EndPoint[R] =
    t.foldLeft(h)((acc, it) =>
      acc catchSome { case None => it }
    )

  def requestStringBody(req: Request): IO[HTTPError, String] =
    req.body match {
      case Some(value) =>
        value.transduce(ZTransducer.utf8Decode).runHead.someOrFail(BadRequest("Missing body"))
      case None => ZIO.fail(BadRequest("Missing body"))
    }
}

package object hrequest {

  def request = ZIO.access[HRequest](_.get)

  def stringBody =
    for {
      req <- request
      s <- EndPoint.requestStringBody(req)
    } yield s

  def uri = request.map { r =>
    r.uri.getPath.split("/").toList.filterNot(_ == "")
  }

  def method = request.map(_.method)

  def webSocket: ZIO[HRequest, HTTPError, WebsocketRequest] =
    for {
      r <- request
      ws <- r match {
        case wr: WebsocketRequest => IO.succeed(wr)
        case _ => IO.fail(BadRequest("not a websocket"))
      }
    } yield ws

  def handleWebsocketFrame(
    textHandler: (String, Boolean) => Stream[HTTPError, Frame],
    onClose: Close.type => Stream[HTTPError, Close.type] = Stream(_),
    onContinuation: (Array[Byte], Boolean)=> Stream[HTTPError, Frame] = (_, _) => Stream.empty,
    onBinary: (Array[Byte], Boolean)=> Stream[HTTPError, Frame] = (_, _) => Stream.empty
  )(frame: Frame): Stream[HTTPError, Frame] = frame match {
    case _@Binary(data, isLast) => onBinary(data, isLast)
    case _@Text(data, isLast) => textHandler(data, isLast)
    case _@Continuation(data, isLast) => onContinuation(data, isLast)
    case Ping => Stream(Pong)
    case Pong => Stream.empty
    case Close => onClose(Close)
  }
}