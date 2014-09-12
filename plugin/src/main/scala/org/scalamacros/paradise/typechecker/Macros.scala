package org.scalamacros.paradise
package typechecker

trait Macros {
  self: Analyzer =>

  import global._
  import scala.language.reflectiveCalls

  def macroExpandUntyped(typer: Typer, expandee: Tree): Option[Tree] = {
    val result = invokeSuper("macroExpand1", typer, expandee)
    if (result.getClass.getName.endsWith("Success")) Some(result.asInstanceOf[{ def expanded: Tree }].expanded)
    else None
  }

  def invokeSuper(name: String, args: AnyRef*): AnyRef = {
    def invokeTraitPrivateMethod(clazz: Class[_], name: String, args: AnyRef*): AnyRef = {
      try {
        val traitImpl = Class.forName(clazz.getName + "$class")
        val meth = traitImpl.getDeclaredMethods.filter(m => m.getName == name || m.getName.endsWith("$" + name)).head
        meth.setAccessible(true)
        meth.invoke(null, args: _*)
      } catch unwrapHandler({ case ex => throw ex })
    }
    invokeTraitPrivateMethod(classOf[scala.tools.nsc.typechecker.Macros], name, (self +: args): _*)
  }
}
