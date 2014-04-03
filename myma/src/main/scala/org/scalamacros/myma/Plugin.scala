package org.scalamacros.myma

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "myma"
  val description = "My migration analyzer"
  val components = List[NscPluginComponent](PluginComponent)

  private object PluginComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "myma"

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
        lazy val deps: Map[Symbol, Tree] = {
          def isTrulyInternal(sym: Symbol): Boolean = {
            val doesntComeFromApi = !sym.allOverriddenSymbols.exists(sym => sym.fullName.startsWith("scala.reflect.api"))
            val doesntComeFromMacros = !sym.allOverriddenSymbols.exists(sym => sym.fullName.startsWith("scala.reflect.macros"))
            val doesntComeFromStdDefs = {
              val comesFromValueClassDefs = sym.owner.name == newTypeName("ValueClassDefinitions")
              val isOneOfStdDefs = typeOf[scala.reflect.api.StandardDefinitions].decl(newTypeName("DefinitionsApi")).info.decls.map(_.name).toSet.contains(sym.name)
              val isOneOfStdTypes = typeOf[scala.reflect.api.StandardDefinitions].decl(newTypeName("StandardTypes")).info.decls.map(_.name).toSet.contains(sym.name)
              !(comesFromValueClassDefs && (isOneOfStdDefs || isOneOfStdTypes))
            }
            doesntComeFromApi && doesntComeFromMacros && doesntComeFromStdDefs
          }
          val deps = unit.body.collect{ case tree if tree.hasSymbol => (tree.symbol, tree) }.toMap
          val relevant = deps.filterKeys(sym => !sym.isPackage && !sym.isModule)
          val internal = relevant.filterKeys(_.fullName.startsWith("scala.reflect.internal."))
          val trulyInternal = internal.filterKeys(isTrulyInternal)
          trulyInternal
        }
        val unauthorized = deps.filterKeys(dep => !whitelist.contains(dep.fullName))
        unauthorized.foreach{ case (dep, usage) => unit.error(usage.pos, s"Usage of unauthorized API: ${dep.fullName}") }
      }
    }
  }
}
