import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

class Repl extends FunSuite {
  private def repl(code: String): String = {
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("sbt.paths.tests.classpath")
    s.plugin.value = List(sys.props("sbt.paths.plugin.jar"))
    val lines = ILoop.runForTranscript(code, s).lines.toList
    lines.drop(3).dropRight(2).map(_.replaceAll("\\s+$","")).mkString("\n").trim.stripSuffix("scala>").trim
  }

  test("precompiled macros expand") {
    assert(repl("""
      |@thingy class Thingy
      |@thingy class NonThingy
    """.stripMargin.trim) === """
      |scala> @thingy class Thingy
      |defined class Thingy
      |defined object Thingy
      |
      |scala> @thingy class NonThingy
      |defined class Thingy
      |defined object Thingy
    """.stripMargin.trim)
  }

  test("ad-hoc macros expand") {
    assert(repl("""
      |import scala.language.experimental.macros
      |import scala.reflect.macros.whitebox.Context
      |import scala.annotation.StaticAnnotation
      |
      |object thingyAdhocMacro {
      |  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      |    import c.universe._
      |    val toEmit = c.Expr(q"class Thingy(i: Int) { def stuff = println(i) }; object Thingy { def apply(x: Int) = new Thingy(x) }")
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
      |scala> import scala.reflect.macros.whitebox.Context
      |import scala.reflect.macros.whitebox.Context
      |
      |scala> import scala.annotation.StaticAnnotation
      |import scala.annotation.StaticAnnotation
      |
      |scala>
      |
      |scala> object thingyAdhocMacro {
      |  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      |    import c.universe._
      |    val toEmit = c.Expr(q"class Thingy(i: Int) { def stuff = println(i) }; object Thingy { def apply(x: Int) = new Thingy(x) }")
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
      |defined object thingyAdhocMacro
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
      |defined object Thingy
      |
      |scala> @thingyAdhoc class NonThingy
      |defined class Thingy
      |defined object Thingy
    """.stripMargin.trim)
  }
}