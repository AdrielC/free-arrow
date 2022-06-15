package com.adrielc.quivr.quasar.util

import com.adrielc.quivr.quasar.Client
import fs2._
import org.http4s.{Header, Headers, Method, Request, Response}

case class RequestUtil[F[_]](c: Client[F]) {

  val tokenHeader: Header = Header("Authorization", c.token)

  def post[A](path: String, headers: List[Header], content: Stream[F, Byte]): Stream[F, Response[F]] = {
    val thisReqPath = c.apiRoot / path
    val req = Request[F](Method.POST, thisReqPath, headers = Headers(tokenHeader :: headers), body = content)
    c.httpClient.stream(req)
  }

  def get(path: String, headers: List[Header]): Stream[F, Response[F]] = {
    val thisReqPath = c.apiRoot / path
    val req = Request[F](Method.GET, thisReqPath, headers = Headers(tokenHeader :: headers))
    c.httpClient.stream(req)
  }

}
