package com.adrielc.freearrow

import cats.effect._
import cats.implicits._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO(println("Hello")).as(ExitCode.Success)
}
