import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class identity extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ???
}
