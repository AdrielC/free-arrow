package com.adrielc.quivr.metrics
package function

sealed abstract class GainFn(val f: Double => Double) extends Fn[Double, Double]
private[metrics] object GainFn {
  case object Pow2    extends GainFn(pow2)
  case object Pow1p1  extends GainFn(powOf(1.1))
  case object Pow1p01 extends GainFn(powOf(1.01))
  case object Id      extends GainFn(identity)
}