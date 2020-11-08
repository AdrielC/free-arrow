package com.adrielc.quivr.metrics
package result

import cats.Contravariant

trait Engagements[A, E] {

  def engagements(a: A): Map[ResultId, Map[E, Int]]
}

object Engagements {
  def apply[A, E](implicit E: Engagements[A, E]): Engagements[A, E] = E

  def instance[A, E](f: A => Map[ResultId, Map[E, Int]]): Engagements[A, E] = f(_)

  trait ToEngagementsOps {
    implicit def toEngagementsOps[A](a: A): EngagementsOps[A] = new EngagementsOps(a)
  }
  implicit class EngagementsOps[A](private val a: A) extends AnyVal {
    def engagementCounts[E](implicit E: Engagements[A, E]): Map[ResultId, Map[E, Int]] = E.engagements(a)
  }

  implicit def engagementsIdentityInstance[E]: Engagements[Map[ResultId, Map[E, Int]], E] = identity

  implicit def engagementsRightTuple[A, B: Engagements[*, E], E]: Engagements[(A, B), E] = ab => Engagements[B, E].engagements(ab._2)

  implicit def contravariantEngagements[E]: Contravariant[Engagements[*, E]] = new Contravariant[Engagements[*, E]] {
    def contramap[A, B](fa: Engagements[A, E])(f: B => A): Engagements[B, E] = a => fa.engagements(f(a))
  }
}
