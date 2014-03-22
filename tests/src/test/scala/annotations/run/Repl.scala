import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

class Repl extends FunSuite {
  private def repl(code: String): String = {
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("classpath.for.repl.tests")
    s.plugin.value = List(sys.props("macroparadise.plugin.jar"))
    val lines = ILoop.runForTranscript(code, s).lines.toList
    lines.drop(3).map(_.replaceAll("\\s+$","")).mkString("\n").trim.stripSuffix("scala>").trim
  }

  test("precompiled macros expand") {
    assert(repl("""
      |@thingy class Thingy
      |@thingy class NonThingy
    """.stripMargin.trim) === """
      |scala> @thingy class Thingy
      |defined class Thingy
      |defined module Thingy
      |
      |scala> @thingy class NonThingy
      |defined class Thingy
      |defined module Thingy
    """.stripMargin.trim)
  }

  test("ad-hoc macros expand") {
    assert(repl("""
      |import scala.language.experimental.macros
      |import scala.reflect.macros.Context
      |import scala.annotation.StaticAnnotation
      |
      |object thingyAdhocMacro {
      |  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      |    import c.universe._
      |    val toEmit = {
      |      import Flag._
      |      val PARAMACCESSOR = (1 << 29).asInstanceOf[Long].asInstanceOf[FlagSet]
      |      c.Expr[Any](Block(List(
      |        ClassDef(
      |          Modifiers(), newTypeName("Thingy"), List(),
      |          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
      |          List(
      |            ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), newTermName("i"), Ident(newTypeName("Int")), EmptyTree),
      |            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List(ValDef(Modifiers(PARAM | PARAMACCESSOR, nme.EMPTY), newTermName("i"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
      |            DefDef(Modifiers(), newTermName("stuff"), List(), List(), TypeTree(), Apply(Ident(newTermName("println")), List(Ident(newTermName("i")))))))),
      |        ModuleDef(
      |          Modifiers(), newTermName("Thingy"),
      |          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
      |          List(
      |            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
      |            DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(),
      |              Apply(Select(New(Ident(newTypeName("Thingy"))), nme.CONSTRUCTOR), List(Ident(newTermName("x"))))))))),
      |        Literal(Constant(()))))
      |    }
      |    annottees.map(_.tree) match {
      |      case Nil => {
      |        c.abort(c.enclosingPosition, "No test target")
      |      }
      |      case (classDeclaration: ClassDef) :: Nil => {
      |        // println("No companion provided")
      |        toEmit
      |      }
      |      case (classDeclaration: ClassDef) :: (companionDeclaration: ModuleDef) :: Nil => {
      |        // println("Companion provided")
      |        toEmit
      |      }
      |      case _ => c.abort(c.enclosingPosition, "Invalid test target")
      |    }
      |  }
      |}
      |
      |class thingyAdhoc extends StaticAnnotation {
      |  def macroTransform(annottees: Any*): Any = macro thingyAdhocMacro.impl
      |}
      |
      |@thingyAdhoc class Thingy
      |@thingyAdhoc class NonThingy
    """.stripMargin.trim) === """
      |scala> import scala.language.experimental.macros
      |import scala.language.experimental.macros
      |
      |scala> import scala.reflect.macros.Context
      |import scala.reflect.macros.Context
      |
      |scala> import scala.annotation.StaticAnnotation
      |import scala.annotation.StaticAnnotation
      |
      |scala>
      |
      |scala> object thingyAdhocMacro {
      |  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      |    import c.universe._
      |    val toEmit = {
      |      import Flag._
      |      val PARAMACCESSOR = (1 << 29).asInstanceOf[Long].asInstanceOf[FlagSet]
      |      c.Expr[Any](Block(List(
      |        ClassDef(
      |          Modifiers(), newTypeName("Thingy"), List(),
      |          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
      |          List(
      |            ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), newTermName("i"), Ident(newTypeName("Int")), EmptyTree),
      |            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List(ValDef(Modifiers(PARAM | PARAMACCESSOR, nme.EMPTY), newTermName("i"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
      |            DefDef(Modifiers(), newTermName("stuff"), List(), List(), TypeTree(), Apply(Ident(newTermName("println")), List(Ident(newTermName("i")))))))),
      |        ModuleDef(
      |          Modifiers(), newTermName("Thingy"),
      |          Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
      |          List(
      |            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
      |            DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(),
      |              Apply(Select(New(Ident(newTypeName("Thingy"))), nme.CONSTRUCTOR), List(Ident(newTermName("x"))))))))),
      |        Literal(Constant(()))))
      |    }
      |    annottees.map(_.tree) match {
      |      case Nil => {
      |        c.abort(c.enclosingPosition, "No test target")
      |      }
      |      case (classDeclaration: ClassDef) :: Nil => {
      |        // println("No companion provided")
      |        toEmit
      |      }
      |      case (classDeclaration: ClassDef) :: (companionDeclaration: ModuleDef) :: Nil => {
      |        // println("Companion provided")
      |        toEmit
      |      }
      |      case _ => c.abort(c.enclosingPosition, "Invalid test target")
      |    }
      |  }
      |}
      |defined module thingyAdhocMacro
      |
      |scala>
      |
      |scala> class thingyAdhoc extends StaticAnnotation {
      |  def macroTransform(annottees: Any*): Any = macro thingyAdhocMacro.impl
      |}
      |defined class thingyAdhoc
      |
      |scala>
      |
      |scala> @thingyAdhoc class Thingy
      |defined class Thingy
      |defined module Thingy
      |
      |scala> @thingyAdhoc class NonThingy
      |defined class Thingy
      |defined module Thingy
    """.stripMargin.trim)
  }
}