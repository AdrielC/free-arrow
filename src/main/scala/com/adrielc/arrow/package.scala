package com.adrielc

package object arrow {

  type ~~>[-F[_, _], +G[_, _]] = FunctionA[F, G]

  /** A  [[~~>]] */
  type ~>|[-F[_, _], +M] = F ~~> λ[(α, β) => M]

  val lzy = data.LazyTuple
  type :&:[A,B] = lzy.LazyTuple2[A,B]
}
