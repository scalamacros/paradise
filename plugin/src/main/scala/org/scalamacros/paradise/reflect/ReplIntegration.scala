package org.scalamacros.paradise
package reflect

import scala.tools.nsc.interpreter._

trait ReplIntegration {
  self: Enrichments =>
  import global._

  def tellReplAboutExpansion(sym: Symbol, companion: Symbol, expanded: List[Tree]): Unit = {
    // TODO: The workaround employed in https://github.com/scalamacros/paradise/issues/19
    // no longer works because of the REPL refactoring in 2.13.0-M2.
    // See https://github.com/scalamacros/paradise/issues/102 for discussion.
  }
}
