package com.adrielc.quivr.quasar

import cats.effect.kernel.Async
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Sync}
import com.adrielc.quivr.quasar.ws.event.{MessageCreate, Ready, ReadyData}
import fs2.Stream

import scala.io.StdIn

object Test extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    program[IO].as(ExitCode.Success)
  }

  def program[F[_]: Async]: F[Unit] = {
    for {
      t <- Sync[F].delay(StdIn.readLine("Token? "))
      c <- Client[F](t)
      l <- c
        .login(
          EventHandler[F] {
            case MessageCreate(_, m) =>
              Stream
                .eval(
                  Sync[F].delay(println(s"${m.author.fold(_.spaces2,
                    _.\\("username").map(_.spaces2)
                      .foldSmash(" ", " ", ""))}: ${m.content}"))
                )
            case Ready(_, ReadyData(_, user, _, _)) =>
              Stream.eval(
                Sync[F].delay(println(show"Ready! Logged in as $user."))
              )
          }
            :: Defaults.defaultEventHandler[F]
            :: Nil
        )
        .compile
        .drain
    } yield l
  }
}
