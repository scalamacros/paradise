package org.scalalang.macroparadise

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.{mutable, immutable}
import org.scalalang.macroparadise.typechecker.AnalyzerPlugins

class Plugin(val global: Global) extends NscPlugin with AnalyzerPlugins {
  import global._

  val name = "macroparadise"
  val description = "Empowers production Scala compiler with latest macro developments"
  val components = List[NscPluginComponent](PluginComponent)
  analyzer.addAnalyzerPlugin(AnalyzerPlugin)
  analyzer.addMacroPlugin(MacroPlugin)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsAfter = List("parser")
    val phaseName = "macroparadise"
    override val description = "let our powers combine"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        // do nothing: everything's already hijacked
      }
    }
  }
}
