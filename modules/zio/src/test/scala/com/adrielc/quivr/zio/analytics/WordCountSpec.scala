package com.adrielc.quivr.zio.analytics

import zio.console.putStrLn
import zio.duration._
import zio.stream.ZSink

object WordCountSpec extends App {

  val ds = DataStream
    .fromLiterals(
      (12L, "quick"),
      (15L, "quick"),
      (30L, "brown"),
      (40L, "brown"),
      (41L, "brown"),
      (42L, "brown"),
      (30L, "brown"),
      (40L, "brown"),
      (41L, "brown"),
      (42L, "brown"),
      (30L, "brown"),
      (40L, "brown"),
      (41L, "brown"),
      (42L, "brown"),
      (30L, "brown"),
      (40L, "brown"),
      (41L, "brown"),
      (42L, "brown")
    )
    .assignTimestamps(_._1)
    .groupBy(v => v.value._2)
    .foldWindow(WindowAssigner.tumbling(10.millis), 0L) { accAndEl =>
      val acc = accAndEl._1
      acc + 1L
    }

  zio.Runtime.default.unsafeRun({

    println("DataStream plan:")
    pprint.pprintln(ds)

    val stream = Local.evalStream(ds)

    stream
      .tap(r => putStrLn(r.toString).provideLayer(zio.console.Console.live))
      .run(ZSink.collectAll[Local.Record[Grouped[String, Windowed[Long]]]])
      .tap(r => putStrLn(r.toString).provideLayer(zio.console.Console.live))
      .as(0)
  }.unit)
}
