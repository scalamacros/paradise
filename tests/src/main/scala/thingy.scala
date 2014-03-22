// http://stackoverflow.com/questions/22549647/whats-the-right-way-to-generate-a-companion-class-de-novo-using-quasiquotes
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

object thingyMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
//     val toEmit = c.Expr(q"""
// class Thingy(i: Int) {
//   def stuff = println(i)
// }

// object Thingy {
//   def apply(x: Int) = new Thingy(x)
// }
// """)
    val toEmit = {
      import Flag._
      val PARAMACCESSOR = (1 << 29).asInstanceOf[Long].asInstanceOf[FlagSet]
      c.Expr[Any](Block(List(
        ClassDef(
          Modifiers(), newTypeName("Thingy"), List(),
          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
          List(
            ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), newTermName("i"), Ident(newTypeName("Int")), EmptyTree),
            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List(ValDef(Modifiers(PARAM | PARAMACCESSOR, nme.EMPTY), newTermName("i"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
            DefDef(Modifiers(), newTermName("stuff"), List(), List(), TypeTree(), Apply(Ident(newTermName("println")), List(Ident(newTermName("i")))))))),
        ModuleDef(
          Modifiers(), newTermName("Thingy"),
          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
          List(
            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
            DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(),
              Apply(Select(New(Ident(newTypeName("Thingy"))), nme.CONSTRUCTOR), List(Ident(newTermName("x"))))))))),
        Literal(Constant(()))))
    }
    annottees.map(_.tree) match {
      case Nil => {
        c.abort(c.enclosingPosition, "No test target")
      }
      case (classDeclaration: ClassDef) :: Nil => {
        // println("No companion provided")
        toEmit
      }
      case (classDeclaration: ClassDef) :: (companionDeclaration: ModuleDef) :: Nil => {
        // println("Companion provided")
        toEmit
      }
      case _ => c.abort(c.enclosingPosition, "Invalid test target")
    }
  }
}

class thingy extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro thingyMacro.impl
}

package pkg {
  class thingy extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro thingyMacro.impl
  }
}
