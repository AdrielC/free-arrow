package com.adrielc.quivr.metrics
package function

sealed abstract class DiscountFn(val f: Int => Double) extends Fn[Int, Double]
private[metrics] object DiscountFn {

  //  we let a = 2 because it has been reported that nDCG with a large logarithm
  //  base is counterintuitive and lacks discriminative power (Sakai 2007d), despite
  //  the fact that this parameter was designed to reflect persis- tence

  case object Log2      extends DiscountFn(log2)
  case object Id        extends DiscountFn(i => i.toDouble)
}