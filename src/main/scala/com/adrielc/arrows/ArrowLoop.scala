package com.adrielc.arrows

import cats.arrow.Arrow
import com.adrielc.data.:&:

trait ArrowLoop[~>[_, _]] extends Arrow[~>] {

  def loop[B,C,D](f: (B :&: D) ~> (C :&: D)): B ~> C
}
