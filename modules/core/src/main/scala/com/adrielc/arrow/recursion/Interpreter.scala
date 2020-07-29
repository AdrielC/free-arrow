package com.adrielc.arrow.recursion

import com.adrielc.arrow.data.~~>

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
sealed trait Interpreter[F[_, _], G[_, _]] extends (F ~~> G) {
  import Interpreter.ComposedInterpreter

  /**
   * A extranatural transformation that will transform an `F[A, B]` for any type `A`, `B`
   * into an `G[A, B]`.
   */
  def interpret: F ~~> G

  final override def compose[H[_, _]](nt: H ~~> F): Interpreter[H, G] = this match {
    case i: ComposedInterpreter[h, G, F] => Interpreter.ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }

  def apply[A, B](f: F[A, B]): G[A, B] = interpret(f)
}

object Interpreter {

  private final case class ComposedInterpreter[F[_, _], G[_, _], H[_, _]](
    underlying: Interpreter[F, G],
    nt: H ~~> F
  ) extends Interpreter[H, G] {
    final override val interpret = underlying.interpret.compose(nt)
  }

  private final class CataInterpreter[S[_[_, _], _, _], F[_, _]](
    algebra: Alg[S, F]
  )(implicit ev: ArFunctor[S])
    extends Interpreter[Fix[S, ?, ?], F] {
    final override val interpret = cataNT(algebra)
  }

  private final class HyloInterpreter[S[_[_, _], _, _], F[_, _], G[_, _]](
    coalgebra: Coalg[S, G],
    algebra: Alg[S, F]
  )(implicit ev: ArFunctor[S])
    extends Interpreter[G, F] {
    final override val interpret = hyloNT(coalgebra, algebra)
  }

  def cata[S[_[_, _], _, _], F[_, _]](
    alg: Alg[S, F]
  )(implicit ev: ArFunctor[S]): Interpreter[Fix[S, ?, ?], F] =
    new CataInterpreter[S, F](alg)

  def hylo[S[_[_, _], _, _], F[_, _], G[_, _]](
                                                coAlg: Coalg[S, G],
                                                alg: Alg[S, F]
  )(implicit ev: ArFunctor[S]): Interpreter[G, F] =
    new HyloInterpreter(coAlg, alg)
}