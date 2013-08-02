package org.scalalang.macroparadise
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}

trait Analyzer extends NscAnalyzer
                  with Namers
                  with Typers {

  import global._

  override def newNamer(context: Context) = new Namer(context) with ParadiseNamer
  override def newTyper(context: Context) = new Typer(context) with ParadiseTyper
}
