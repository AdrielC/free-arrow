package com.adrielc.quivr.quasar

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.effect.kernel.Async
import com.adrielc.quivr.quasar.ws.event.{MessageCreate, Ready, ReadyData}
import fs2.Stream
import cats.implicits._

object ScheduleTest extends IOApp {



  override def run(args: List[String]): IO[ExitCode] = {
    program[IO].as(ExitCode.Success)
  }

  def program[F[_]: Async]: F[Unit] = {
    for {
      t <- Sync[F].pure("MjAwMTA2OTA3MDk5MzMyNjA5.GYMEbY.a_8xhdWHcqshd8jTbTkUD_ZK4djocTBHsuJDAo")
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
