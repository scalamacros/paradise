package org.scalalang.macroparadise
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import org.scalalang.macroparadise.reflect.Enrichments

trait Analyzer extends NscAnalyzer
                  with Namers
                  with Typers
                  with Enrichments
                  with ContextErrors
                  with AnalyzerPlugins
                  with Macros {

  override def newNamer(context: Context) = new Namer(context) with ParadiseNamer
  override def newTyper(context: Context) = new Typer(context) with ParadiseTyper
  addAnalyzerPlugin(ParadiseAnalyzerPlugin)
}
