package com.adrielc.quivr.quasar.util

import com.adrielc.quivr.quasar.Client
import fs2._
import org.http4s.{Header, Headers, Method, Request, Response}
import org.typelevel.ci.CIStringSyntax

case class RequestUtil[F[_]](c: Client[F]) {

  val tokenHeader: Header.Raw = Header.Raw(ci"Authorization", c.token)

  def post[A](path: String, headers: List[Header.Raw], content: Stream[F, Byte]): Stream[F, Response[F]] = {
    val thisReqPath = c.apiRoot / path
    val req = Request[F](Method.POST, thisReqPath, headers = Headers(tokenHeader :: headers), body = content)
    c.httpClient.stream(req)
  }

  def get(path: String, headers: List[Header.Raw]): Stream[F, Response[F]] = {
    val thisReqPath = c.apiRoot / path
    val req = Request[F](Method.GET, thisReqPath, headers = Headers(tokenHeader :: headers))
    c.httpClient.stream(req)
  }
}
