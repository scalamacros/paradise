package org.scalalang.macroparadise

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.{mutable, immutable}
import org.scalalang.macroparadise.typechecker.Analyzer

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "macroparadise"
  val description = "Empowers production Scala compiler with latest macro developments"
  val components = List[NscPluginComponent](PluginComponent)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsAfter = List("parser")
    val phaseName = "macroparadise"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("ASSIMILATION SUCCESSFUL!")
      }
    }
  }

  override def processOptions(options: List[String], error: String => Unit) {
    options foreach {
      case option => error("Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some("""
  """.trim.stripMargin)
}
