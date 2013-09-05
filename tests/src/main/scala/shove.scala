import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object shoveMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val Apply(Select(New(AppliedTypeTree(_, List(victim))), _), _) = c.prefix.tree
    val result = {
      annottees.map(_.tree).toList match {
        case ValDef(mods, name, tpt, rhs) :: Nil =>
          ClassDef(
            Modifiers(IMPLICIT),
            newTypeName(s"${victim}With$name"),
            List(),
            Template(
              List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))),
              emptyValDef,
              List(
                ValDef(Modifiers(PRIVATE | LOCAL), newTermName("x"), victim, EmptyTree),
                DefDef(
                  Modifiers(),
                  nme.CONSTRUCTOR,
                  List(),
                  List(List(ValDef(Modifiers(PARAM), newTermName("x"), victim, EmptyTree))),
                  TypeTree(),
                  Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), c.literalUnit.tree)
                ),
                ValDef(mods, name, tpt, rhs)
              )
            )
          )
      }
    }
    c.Expr[Any](result)
  }
}

class shove[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro shoveMacro.impl
}

package pkg {
  class shove extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro shoveMacro.impl
  }
}