package com.adrielc

import com.adrielc.arrow.data.LazyTuple

package object arrow {

  type ~~>[-F[_, _], +G[_, _]] = FunctionBiK[F, G]
  type :&:[A,B] = LazyTuple.LazyTuple2[A,B]
  val lazyTuple = LazyTuple
}
