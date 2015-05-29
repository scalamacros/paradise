package org.scalamacros.paradise
package typechecker

import org.scalamacros.paradise.reflect.Enrichments

trait AnalyzerPlugins extends Compilers
                        with Namers
                        with Expanders
                        with Errors
                        with Enrichments
{
  import global._
  import scala.reflect.internal.Flags._
  import analyzer._
  import analyzer.{Namer => NscNamer, AnalyzerPlugin => NscAnalyzerPlugin, MacroPlugin => NscMacroPlugin}

  object AnalyzerPlugin extends NscAnalyzerPlugin {
    override def pluginsTypeSig(tpe: Type, typer: Typer, tree: Tree, pt: Type) = {
      tree match {
        case Template(_, _, body) =>
          mkExpander(typer.namer).expandMacroAnnotations(body)
        case cdef @ ClassDef(_, _, _, _) if { cdef.symbol.setInfo(tpe); treeInfo.isMacroAnnotation(cdef) } =>
          mkCompiler(typer).typedMacroAnnotation(cdef)
        case _ =>
          // do nothing
      }
      tpe
    }
  }

  object MacroPlugin extends NscMacroPlugin {
    override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] =
      mkExpander(typer.namer).expandMacroAnnotations(stats)

    override def pluginsEnterSym(namer: NscNamer, tree: Tree) =
      { mkNamer(namer).enterSym(tree); true }

    override def pluginsEnsureCompanionObject(namer: NscNamer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)) =
      Some(mkNamer(namer).ensureCompanionObject(cdef, creator))

    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = {
      if (ddef.name == nme.macroTransform && typer.context.owner.owner.hasFlag(MACRO)) {
        val result = standardTypedMacroBody(typer, ddef)
        loadMacroImplBinding(ddef.symbol).map(binding => {
          val message =
            "implementation restriction: macro annotation impls cannot have typetag context bounds " +
            "(consider taking apart c.macroApplication and manually calling c.typecheck on the type arguments)"
          val hasTags = binding.signature.flatten.exists(_.isTag)
          if (hasTags) { typer.context.error(ddef.pos, message); EmptyTree }
          else result
        })
      } else {
        None
      }
    }
  }
}
