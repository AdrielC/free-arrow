package com.adrielc.arrow

sealed trait MaybeFn[-A, +B] extends (A => Option[B]) {
  override def apply(v1: A): Option[B] = this match {
    case MaybeFn.Op(f) => Some(f(v1))
    case MaybeFn.No => None
  }
}

object MaybeFn {

  def apply[A, B](f: A => B): MaybeFn[A, B] = Op(f)

  case class Op[A, B](f: A => B) extends MaybeFn[A, B]
  case object No extends MaybeFn[Any, Nothing]

  implicit val arrowPlus: ArrowPlus[MaybeFn] = new ArrowPlus[MaybeFn] {

    def plus[A, B](f: MaybeFn[A, B], g: MaybeFn[A, B]): MaybeFn[A, B] = if(f == No) g else f

    def zeroArrow[B, C]: MaybeFn[B, C] = No

    def lift[A, B](f: A => B): MaybeFn[A, B] = Op(f)

    def compose[A, B, C](f: MaybeFn[B, C], g: MaybeFn[A, B]): MaybeFn[A, C] = (f, g) match {
      case (Op(f), Op(g)) => Op(f compose g)
      case _ => No
    }

    def first[A, B, C](fa: MaybeFn[A, B]): MaybeFn[(A, C), (B, C)] = fa match {
      case Op(f) => Op(t => (f(t._1), t._2))
      case No => No
    }
  }
}

