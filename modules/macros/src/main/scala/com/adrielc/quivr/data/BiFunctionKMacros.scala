package com.adrielc.quivr.data

import scala.reflect.macros.blackbox

private[quivr] object BiFunctionKMacros {

  def lift[BiFunctionK[_[_, _], _[_, _]], F[_, _], G[_, _]](c: blackbox.Context)(
    f: c.Expr[(F[α, β] => G[α, β]) forSome { type α; type β }]
  )(
                              implicit evF: c.WeakTypeTag[F[_, _]],
                              evG: c.WeakTypeTag[G[_, _]]
                            ): c.Expr[BiFunctionK[F, G]] =
    c.Expr[BiFunctionK[F, G]](new Lifter[c.type](c).lift[F, G](f.tree))
  // ^^note: extra space after c.type to appease scalastyle

  private[this] class Lifter[C <: blackbox.Context](val c: C) {
    import c.universe._

    def lift[F[_, _], G[_, _]](tree: Tree)(
      implicit evF: c.WeakTypeTag[F[_, _]],
      evG: c.WeakTypeTag[G[_, _]]
    ): Tree = unblock(tree) match {
      case q"($param) => $trans[..$typeArgs](${arg: Ident})" if param.name == arg.name =>

        typeArgs
          .collect { case tt: TypeTree => tt }
          .find(tt => Option(tt.original).isDefined)
          .foreach { param =>
            c.abort(param.pos, s"type parameter $param must not be supplied when lifting function $trans to BiFunctionK")
          }

        val F = punchHole(evF.tpe)
        val G = punchHole(evG.tpe)

        q"""
        new _root_.com.adrielc.quivr.data.BiFunctionK[$F, $G] {
          def apply[A, B](fab: $F[A, B]): $G[A, B] = $trans(fab)
        }
       """
      case other =>
        c.abort(other.pos, s"Unexpected tree $other when lifting to BiFunctionK")
    }

    private[this] def unblock(tree: Tree): Tree = tree match {
      case Block(Nil, expr) => expr
      case _                => tree
    }

    private[this] def punchHole(tpe: Type): Tree = tpe match {
      case PolyType(alpha :: beta :: Nil, underlying: TypeRef) =>
        val α = TypeName("α")
        val β = TypeName("β")
        def rebind(typeRef: TypeRef): Tree = {
          if (typeRef.sym == alpha) tq"$α"
          else if (typeRef.sym == beta) tq"$β"
          else {
            val args = typeRef.args.map {
              case ref: TypeRef => rebind(ref)
              case arg          => tq"$arg"
            }
            tq"${typeRef.sym}[..$args]"
          }
        }
        val rebound = rebind(underlying)
        tq"""({type λ[$α, $β] = ${rebound}})#λ"""
      case TypeRef(pre@_, sym, Nil) =>
        tq"$sym"
      case _ =>
        c.abort(c.enclosingPosition, s"Unexpected type $tpe when lifting to FunctionK")
    }

  }
}
