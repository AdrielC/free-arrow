package com.adrielc.quivr.metrics
package result

import cats.{Contravariant, Inject}
import shapeless.=:!=

trait Engagements[-A, E] {

  def engagements(a: A): Map[ResultId, Map[E, Int]]
}

object Engagements extends EngagementsInject {

  def apply[A, E](implicit E: Engagements[A, E]): Engagements[A, E] = E

  def instance[A, E](f: A => Map[ResultId, Map[E, Int]]): Engagements[A, E] = f(_)

  trait ToEngagementsOps {
    implicit def toEngagementsOps[A](a: A): EngagementsOps[A] = new EngagementsOps(a)
  }
  implicit class EngagementsOps[A](private val a: A) extends AnyVal {
    def engagements[E](implicit E: Engagements[A, E]): Map[ResultId, Map[E, Int]] = E.engagements(a)
  }

  implicit def engagementsRightTuple[A, B: Engagements[*, E], E]: Engagements[(A, B), E] = ab => Engagements[B, E].engagements(ab._2)

  implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
    def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagements(f(a))
  }
}

trait EngagementsInject {

  implicit def engagementsIdentityInstance[E]: Engagements[Map[ResultId, Map[E, Int]], E] = identity

  implicit def injectEngagements[A, E, E1, E2]
  (implicit
   AE1: Engagements[A, E1],
   AE2: Engagements[A, E2],
   E1: E1 Inject E,
   E2: E2 Inject E,
   nonEq: E1 =:!= E2
  ): Engagements[A, E] = Engagements.instance { a =>
    val _ = nonEq
    import cats.implicits._
    val e1 = AE1.engagements(a).mapValues(_.map { case (k, v) => E1.inj(k) -> v })
    val e2 = AE2.engagements(a).mapValues(_.map { case (k, v) => E2.inj(k) -> v })
    e1 |+| e2
  }
}
