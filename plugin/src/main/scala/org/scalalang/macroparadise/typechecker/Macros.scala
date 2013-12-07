package org.scalalang.macroparadise
package typechecker

import scala.tools.nsc.typechecker.{Macros => NscMacros}

trait Macros extends NscMacros {
  self: Analyzer =>

  import global._
  import scala.language.reflectiveCalls
  import scala.reflect.internal.util.AbstractFileClassLoader
  import scala.reflect.internal.Mode

  // named like this to make sure that macro-generated exceptions are printed in a sane way
  // TODO: there's still a problem with stack traces, because they include reflective frames from meth.invoke
  // however that's better than nothing
  def macroExpand1[T](body: => T): T = body

  def expandUntyped(typer: Typer, expandee: Tree): Option[Tree] = {
    import typer.TyperErrorGen._
    def invokeTraitPrivateMethod(clazz: Class[_], name: String, args: AnyRef*): AnyRef = {
      try {
        val traitImpl = Class.forName(clazz.getName + "$class")
        val meth = traitImpl.getDeclaredMethods.filter(_.getName.endsWith("$" + name)).head
        meth.invoke(null, args: _*)
      } catch unwrapHandler({ case ex => throw ex })
    }
    def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): Option[Tree] = macroExpand1 {
      val result = invokeTraitPrivateMethod(classOf[scala.tools.nsc.typechecker.Macros], "macroExpandWithRuntime", self, typer, expandee, runtime)
      result match {
        case null => None
        case result if result.getClass.getName.endsWith("Success") => Some(result.asInstanceOf[{ val expanded: Tree }].expanded)
        case _ => None
      }
    }

    try {
      if (expandee.symbol.isErroneous) return None
      val runtime = macroRuntime(expandee.symbol)
      assert(runtime != null, expandee)
      macroExpandWithRuntime(typer, expandee, runtime)
    } catch {
      case MacroExpansionException => None
    }
  }

  protected override def findMacroClassLoader(): ClassLoader = {
    val loader = super.findMacroClassLoader
    if (globalSettings.exposeEmptyPackage) {
      // FIXME: i wonder why ReplGlobal.findMacroClassLoader doesn't kick in
      macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(global.classPath.asURLs))
      val virtualDirectory = globalSettings.outputDirs.getSingleOutput.get
      new AbstractFileClassLoader(virtualDirectory, loader) {}
    } else {
      loader
    }
  }
}
