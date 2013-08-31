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
                  with Macros
                  with FastTrack {

  override def newNamer(context: Context) = new Namer(context) with ParadiseNamer
  override def newTyper(context: Context) = new Typer(context) with ParadiseTyper
  addAnalyzerPlugin(ParadiseAnalyzerPlugin)

  def init() = {
    // heuristics to detect situations when hijacking fast track is inappropriate
    // TODO: replace this with something more robust
    import global._
    import definitions._
    def haveScalaReflect = ApiUniverseClass != NoSymbol
    def compilingScalaReflect = {
      val universeDotScala = "src/reflect/scala/reflect/api/Universe.scala".replace("/", java.io.File.separator)
      currentRun.compiledFiles.exists(fname => fname != null && fname.endsWith(universeDotScala))
    }

    if (haveScalaReflect && !compilingScalaReflect) {
      paradiseDefinitions.init()
      fastTrack.hijack()
    } else {
      if (settings.Ydebug.value) {
        val part1 = "Macro paradise plugin couldn't initialize the quasiquoting module (haveScalaReflect = $haveScalaReflect, compilingScalaReflect = $compilingScalaReflect)"
        val part2 = "Lodge an issue at https://github.com/scalamacros/paradise/issues if this is a problem for you"
        Console.err.println(s"$part1. $part2")
      }
    }
  }
}
