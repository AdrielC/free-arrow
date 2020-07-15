package com.adrielc.arrow.free

import implicits._
import cats.implicits._
import com.adrielc.arrow.examples.ConsoleArr._

object ArrMain extends App {

  val printInt = ~GetInt >^ (_.toString) >>^ PutLine

  val f = printInt foldMap functionInterpreter

  f(())
}
