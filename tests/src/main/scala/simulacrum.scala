import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object simulacrumMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(()))))
  }
}

class simulacrum extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro simulacrumMacro.impl
}