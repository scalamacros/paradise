package org.scalalang.macroparadise
package reflect

trait StdNames {
  self: Enrichments =>
  import global._

  object paradiseNme {
    val annottees: TermName      = "annottees"
    val macroTransform: TermName = "macroTransform"
  }
}
