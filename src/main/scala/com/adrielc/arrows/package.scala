package com.adrielc

package object arrows {

  type ~~>[-F[_, _], +G[_, _]] = FunctionBiK[F, G]
}
