package com.adrielc.quivr.metrics

import cats.data.NonEmptySet
import com.adrielc.quivr.metrics.data.ResultId

object types {

  // relevant document set
  case class QrelSet(qrelSet: NonEmptySet[ResultId]) {

    lazy val nRel: Int = qrelSet.length
  }
}
