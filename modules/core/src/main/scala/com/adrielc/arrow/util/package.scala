package com.adrielc.arrow

package object util {

  type ∀[+F[_[_, _]]] = Forall[F]
  val ∀ = Forall
}
