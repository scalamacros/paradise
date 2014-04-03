package org.scalamacros.myma

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "myma"
  val description = "My migration analyzer"
  val components = List[NscPluginComponent](LogicalComponent, PhysicalComponent)

  private object LogicalComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "myma-logical"

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        lazy val whitelist: Set[String] = {
          // TODO: looks like using JAR resources requires closing and reopening SBT when the resource changes
          // val resourceStream = getClass.getResourceAsStream("/whitelist.conf")
          // if (resourceStream == null) abort("couldn't load whitelist.conf")
          // io.Source.fromInputStream(resourceStream).getLines.toSet
          val resourcePath = System.getProperty("myma.whitelist.conf")
          if (resourcePath == null) abort("couldn't load whitelist.conf")
          io.Source.fromFile(resourcePath).getLines.toSet
        }
        lazy val deps: Map[Symbol, List[Tree]] = {
          def isTrulyInternal(sym: Symbol): Boolean = {
            val doesntComeFromApi = !sym.allOverriddenSymbols.exists(sym => sym.fullName.startsWith("scala.reflect.api"))
            val doesntComeFromMacros = !sym.allOverriddenSymbols.exists(sym => sym.fullName.startsWith("scala.reflect.macros"))
            doesntComeFromApi && doesntComeFromMacros
          }
          val deps = unit.body.collect{ case tree if tree.hasSymbol => (tree.symbol, tree) }.groupBy(_._1).mapValues(v => v.map(_._2))
          val relevant = deps.filterKeys(sym => !sym.isPackage && !sym.isModule)
          val internal = relevant.filterKeys(_.fullName.startsWith("scala.reflect.internal."))
          val trulyInternal = internal.filterKeys(isTrulyInternal)
          trulyInternal
        }
        val unauthorized = deps.filterKeys(dep => !whitelist.contains(dep.fullName))
        unauthorized.foreach{ case (dep, usages) => usages.foreach(usage => unit.error(usage.pos, s"Usage of unauthorized API: ${scala.reflect.NameTransformer.decode(dep.fullName)}")) }
      }
    }
  }

  private object PhysicalComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("cleanup")
    val phaseName = "myma-physical"

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        lazy val entirelist: Set[String] = {
          val resourcePath = System.getProperty("myma.entirelist.conf")
          if (resourcePath == null) abort("couldn't load entirelist.conf")
          io.Source.fromFile(resourcePath).getLines.toSet
        }
        lazy val deps: Map[Symbol, List[Tree]] = {
          val deps = unit.body.collect{ case tree if tree.hasSymbol => (tree.symbol, tree) }.groupBy(_._1).mapValues(v => v.map(_._2))
          deps.filterKeys(_.fullName.startsWith("scala.reflect.internal."))
        }
        val unauthorized = deps.filterKeys(dep => !entirelist.contains(dep.fullName))
        unauthorized.foreach{ case (dep, usages) => usages.foreach(usage => unit.error(usage.pos, s"Usage of unauthorized API: ${scala.reflect.NameTransformer.decode(dep.fullName)}")) }
      }
    }
  }
}
