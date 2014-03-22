package org.scalamacros.paradise
package quasiquotes

import scala.reflect.macros.runtime.Context
import org.scalamacros.paradise.reflect.Enrichments

abstract class Quasiquotes extends Parsers
                              with Holes
                              with Placeholders
                              with Reifiers
                              with Enrichments {
  val c: Context
  val global: c.universe.type = c.universe
  import c.universe._
  import paradiseDefinitions._
  import compat.build._

  def debug(msg: => String): Unit =
    if (settings.Yquasiquotedebug.value) println(msg)

  lazy val (universe: Tree, args, parts, parse, reify, method) = c.macroApplication match {
    case Apply(SyntacticTypeApplied(Select(Select(Apply(Select(universe0, _), List(Apply(_, parts0))), interpolator0), method0), _), args0) =>
      debug(s"parse prefix:\nuniverse=$universe0\nparts=$parts0\ninterpolator=$interpolator0\nmethod=$method0\nargs=$args0\n")
      val parts1 = parts0.map {
        case lit @ Literal(Constant(s: String)) => s -> lit.pos
        case part => c.abort(part.pos, "Quasiquotes can only be used with literal strings")
      }
      val reify0 = method0 match {
        case nme.apply   => new ApplyReifier().reifyFillingHoles(_)
        case nme.unapply => new UnapplyReifier().reifyFillingHoles(_)
        case other       => global.abort(s"Unknown quasiquote api method: $other")
      }
      val parse0 = interpolator0 match {
        case paradisenme.q  => TermParser.parse(_)
        case paradisenme.tq => TypeParser.parse(_)
        case paradisenme.cq => CaseParser.parse(_)
        case paradisenme.pq => PatternParser.parse(_)
        case paradisenme.fq => ForEnumeratorParser.parse(_)
        case other          => global.abort(s"Unknown quasiquote flavor: $other")
      }
      (universe0, args0, parts1, parse0, reify0, method0)
    case _ =>
      global.abort(s"Couldn't parse call prefix tree ${c.macroApplication}.")
  }

  lazy val u = universe // shortcut
  lazy val universeTypes = new paradiseDefinitions.UniverseDependentTypes(universe)

  def expandQuasiquote = {
    if (QuasiquoteCompatModule == NoSymbol) {
      val message =
        "Quasiquotes in macro paradise for Scala 2.10 now require a dependency on a supporting library. " +
        "Add the following line to your SBT build: " +
        """`libraryDependencies += "org.scalamacros" %% "quasiquotes" % "2.0.0-SNAPSHOT" cross CrossVersion.full`"""
      c.abort(c.enclosingPosition, message)
    }

    debug(s"macro application:\n${c.macroApplication}\n")
    debug(s"code to parse:\n$code\n")
    val tree = parse(code)
    debug(s"parsed:\n${showRaw(tree)}\n$tree\n")
    val reified = reify(tree)
    def sreified =
      reified
        .toString
        .replace("scala.reflect.runtime.`package`.universe.", "")
        .replace("scala.quasiquotes.QuasiquoteCompat.apply[type](scala.reflect.runtime.`package`.universe).build.", "")
        .replace("scala.collection.immutable.", "")
    debug(s"reified tree:\n$sreified\n")
    reified
  }
}
