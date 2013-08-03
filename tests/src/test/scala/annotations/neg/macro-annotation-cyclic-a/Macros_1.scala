import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object xrefCyclistMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val name = annottees.head.tree.asInstanceOf[MemberDef].name.toString
    val a = c.mirror.staticClass("A")
    val b = c.mirror.staticClass("B")
    if (name == "A") b.typeSignature
    if (name == "B") a.typeSignature
    c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant())))
  }
}

class xrefCyclist extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro xrefCyclistMacro.impl
}

object introspectCyclistMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val name = annottees.head.tree.asInstanceOf[MemberDef].name.toString
    val cclass = c.mirror.staticClass("C")
    println(cclass.typeSignature)
    c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant())))
  }
}

class introspectCyclist extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro introspectCyclistMacro.impl
}
