import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object identityMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    ???
  }
}

class identity extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro identityMacro.impl
}
