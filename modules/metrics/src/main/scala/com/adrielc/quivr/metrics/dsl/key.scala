package com.adrielc.quivr.metrics.dsl

import cats.data.NonEmptyList
import cats.{Foldable, Semigroup}
import com.adrielc.quivr.~>|
import cats.implicits._
import cats.kernel.Monoid

object key {

  final case class KeyBuilder[A](prefix: Option[A] = None,
                                 delim: A,
                                 suffix: Option[A] = None) {

    def buildKey[F[_]](fa: F[A])(implicit F: Foldable[F], M: Monoid[A]): A = fa.foldSmash(prefix.getOrElse(M.empty), delim, suffix.getOrElse(M.empty))

    def addPrefix(p: A)(implicit S: Semigroup[A]): KeyBuilder[A] = copy(prefix = Option(p) |+| prefix.map(delim |+| _))
    def -:(p: A)(implicit S: Semigroup[A]): KeyBuilder[A] = addPrefix(p)

    def addSuffix(s: A)(implicit S: Semigroup[A]): KeyBuilder[A] = copy(suffix = suffix.map(_ |+| delim) |+| Option(s))
    def :-(s: A)(implicit S: Semigroup[A]): KeyBuilder[A] = addSuffix(s)
  }
  object KeyBuilder {

    def apply[A](a: A): KeyBuilder[A] = KeyBuilder(delim = a)

    implicit def asDelim(k: String): KeyBuilder[String] = KeyBuilder(k)
  }

  case class SummarizeOps[F[_, _], M: Monoid](
    toKey         : F ~>| M,
    toKeyGroup    : F ~>| Int,
    buildKeyGroups: KeyBuilder[M],
    buildFullKey  : KeyBuilder[M]
  ) {

    def summarize(f: List[F[_, _]]): M = {
      val groups = f
        .groupByNel(toKeyGroup(_))
        .map {
          case (_, NonEmptyList(k, Nil)) => toKey(k)
          case (_, nel) => buildKeyGroups.buildKey(nel.map(toKey(_)))
        }
        .toList

      buildFullKey.buildKey(groups)
    }
  }

  trait KeyBuilderSyntax {
    implicit def toKeyBuilderOps[A](a: A): KeyBuilderOps[A] = new KeyBuilderOps(a)
  }

  implicit class KeyBuilderOps[A](private val delim: A) extends AnyVal {
    def -:(p: A)(implicit S: Semigroup[A]): KeyBuilder[A] = KeyBuilder(delim).addPrefix(p)
    def :-(s: A)(implicit S: Semigroup[A]): KeyBuilder[A] = KeyBuilder(delim).addSuffix(s)
  }
}
