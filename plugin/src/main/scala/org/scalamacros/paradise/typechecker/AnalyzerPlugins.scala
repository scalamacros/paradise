package org.scalamacros.paradise
package typechecker

trait AnalyzerPlugins {
  self: Analyzer =>
  import global._

  object ParadiseAnalyzerPlugin extends AnalyzerPlugin {
    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
      defTree match {
        case Template(_, _, body) => newNamer(typer.context).expandMacroAnnotations(body)
        case _ => // do nothing
      }
      tpe
    }
  }
}
