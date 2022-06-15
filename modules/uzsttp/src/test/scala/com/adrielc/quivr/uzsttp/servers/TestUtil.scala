package com.adrielc.quivr
package uzsttp
package servers

import zio._
import uzhttp.server.Server
import java.net.InetSocketAddress

import uzsttp.servers.EndPoint.{EndPoint, HRequest}
import EndPoint._
import uzhttp.{HTTPError, Request, Response}

object TestUtil {

  type Res = zio.blocking.Blocking with zio.clock.Clock

  type HasServer = Has[Server]

  def serverLayer(handler: PartialFunction[Request, ZIO[Res, HTTPError, Response]]): RLayer[Res, HasServer] =
    ZLayer.fromManaged(
    Server.builder(new InetSocketAddress("127.0.0.1", 8080))
      .handleSome(handler)
      .serve
  )

  def serverLayer2(p: EndPoint[HRequest]) = ZLayer.fromManaged(
    Server.builder(new InetSocketAddress("127.0.0.1", 8080))
      .handleAll(a => noAuthHandler(p)(a): ZIO[Res, HTTPError, Response])
      .serve
  )

  def serverLayer2M[R](zEndPoint: RIO[R, EndPoint[HRequest]]) = ZLayer.fromManaged {
    val zm = zEndPoint.map { p =>
      Server.builder(new InetSocketAddress("127.0.0.1", 8080))
        .handleAll(a => noAuthHandler(p)(a): ZIO[Res, HTTPError, Response])
        .serve
    }
    ZManaged.unwrap(zm)
  }

  def serverLayerM[R](handlerM: RIO[R, PartialFunction[Request, IO[HTTPError, Response]]]): RLayer[R with Res, HasServer] =
    ZLayer.fromManaged {
      val zm = handlerM.map { handler =>
        Server.builder(new InetSocketAddress("127.0.0.1", 8080))
          .handleSome(PartialFunction(handler.apply(_): ZIO[R, HTTPError, Response]))
          .serve
      }
      ZManaged.unwrap(zm)
    }

  type UZServer = Has[Server]

  def serverUp = ZIO.access[UZServer](_.get).map{_.awaitUp}
}