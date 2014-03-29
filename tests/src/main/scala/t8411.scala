import scala.language.experimental.macros
import scala.reflect.macros.Context

object t8411 {
  def defaultZeroCase(pf: PartialFunction[Int, Int]): PartialFunction[Int, Int] = macro impl
  def impl(c: Context)(pf: c.Expr[PartialFunction[Int, Int]]): c.Expr[PartialFunction[Int, Int]] = {
    import c.universe._
    val q"{ case ..$cases }" = pf.tree
    c.Expr[PartialFunction[Int, Int]](q"{ case ..$cases case _ => 0 }")
  }
}
