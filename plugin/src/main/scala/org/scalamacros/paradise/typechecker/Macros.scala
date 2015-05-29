package org.scalamacros.paradise
package typechecker

trait Macros {
  self: Analyzer =>

  import global._
  import definitions._
  import scala.language.reflectiveCalls
  import scala.reflect.internal.Flags._

  override def typedMacroBody(typer: Typer, ddef: DefDef): Tree = {
    val result = invokeSuper("typedMacroBody", typer, ddef).asInstanceOf[Tree]
    val clazz = typer.context.owner.owner
    val isMacroAnnot = clazz isNonBottomSubClass AnnotationClass // NOTE: that's an approximation, see 2.11.x
    if (ddef.name == nme.macroTransform && isMacroAnnot) {
      val binding = loadMacroImplBinding(ddef.symbol)
      val message =
        "implementation restriction: macro annotation impls cannot have typetag context bounds " +
        "(consider taking apart c.macroApplication and manually calling c.typecheck on the type arguments)"
      val hasTags = binding.signature.exists(_ >= 0)
      if (hasTags) { typer.context.error(ddef.pos, message); EmptyTree }
      else result
    } else {
      result
    }
  }

  private case class MacroImplBinding(signature: List[Int])
  private def loadMacroImplBinding(macroDef: Symbol): MacroImplBinding = {
    val superBinding = invokeSuper("loadMacroImplBinding", macroDef).asInstanceOf[{ val signature: List[Int] }]
    MacroImplBinding(superBinding.signature)
  }

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
