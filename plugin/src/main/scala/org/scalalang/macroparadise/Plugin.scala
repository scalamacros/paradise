package org.scalalang.macroparadise

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.{mutable, immutable}
import org.scalalang.macroparadise.typechecker.AnalyzerPlugins

class Plugin(val global: Global) extends NscPlugin with AnalyzerPlugins {
  import global._

  val name = "macroparadise"
  val description = "Empowers production Scala compiler with latest macro developments"
  val components = Nil
  analyzer.addAnalyzerPlugin(AnalyzerPlugin)
  analyzer.addMacroPlugin(MacroPlugin)
}
