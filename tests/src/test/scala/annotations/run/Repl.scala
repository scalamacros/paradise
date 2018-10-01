import org.scalatest.FunSuite

import scala.io.AnsiColor._
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

class Repl extends FunSuite {
  private def repl(code: String): String = {
    val oldColorOk = replProps.colorOk
    val f_colorOk = replProps.getClass.getDeclaredField("colorOk")
    f_colorOk.setAccessible(true)
    f_colorOk.set(replProps, false)
    try {
      val s = new Settings
      s.Xnojline.value = true
      s.usejavacp.value = false
      s.classpath.value = sys.props("sbt.paths.tests.classpath")
      s.plugin.value = List(sys.props("sbt.paths.plugin.jar"))
      // Predef.augmentString = work around scala/bug#11125 on JDK 11
      val lines = Predef.augmentString(ILoop.runForTranscript(code, s)).lines.toList
      // TODO: I don't know why but setting replProps.colorOk to false doesn't help.
      // I don't have time to figure out what's going on, so I'll do simple string replacement.
      def cleanup(line: String) = line.replace(MAGENTA, "").replace(RESET, "").trim
      lines.drop(3).dropRight(2).map(cleanup).mkString("\n").trim.stripSuffix("scala>").trim
    } finally {
      f_colorOk.set(replProps, oldColorOk)
    }
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
    val printout = repl("""
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
    """.stripMargin.trim)
    assert(printout.contains("defined class Thingy"))
    assert(printout.contains("defined object Thingy"))
    assert(!printout.contains("defined class NonThingy"))
    assert(!printout.contains("defined object NonThingy"))
  }
}
