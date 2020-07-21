package com.adrielc.arrow.util

import scala.annotation.unchecked.uncheckedVariance

private[util] sealed trait Parent extends Serializable {
  private[util] type Apply[_[_, _]]
}

trait Forall[+F[_[_, _]]] extends Parent { outer =>
  private[util] type Apply[f[_, _]] = F[f] @uncheckedVariance
  def apply[G[_, _]]: F[G]
}

object Forall {
  // cannot be referenced outside of Forall
  private[Forall] type τ[_, _]

  /**
   * This function is intended to be invoked explicitly, the implicit
   * modifiers are simply because the compiler can infer this in many
   * cases. For example:
   *
   * implicit def eitherMonad[A]: Monad[Either[A, ?]] = ???
   *
   * implicitly[∀[α => Monad[Either[α, ?]]]]
   *
   * The above will work.
   */
  def apply[R[_[_, _]]](ft: R[τ]): Forall[R] =
    new Forall[R] {
      def apply[F[_, _]] = ft.asInstanceOf[R[F]]
    }

  /**
   * This is the implicit version of apply, but restructured and encoded
   * such that the F is unconstrained in in arity or fixity.
   */
  implicit def materialize[T <: Parent](implicit ft: T#Apply[τ]): T =
    apply(ft).asInstanceOf[T]

  /**
   * Utilities to implicitly materialize let-bound polymorphic contexts.
   */
  /*object Implicits {
    implicit def materializeUnification[P <: Parent, A, E](
        implicit P: P,
        ev: P#Apply[E] <:< A): A =
      ev(P.asInstanceOf[Forall[P.Apply]][E])
  }*/
}