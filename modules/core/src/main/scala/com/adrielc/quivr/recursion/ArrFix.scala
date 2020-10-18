package com.adrielc.quivr.recursion

final case class ArrFix[R[_[_, _], _, _], A, B](unFix: R[ArrFix[R, *, *], A, B])
object ArrFix {

}