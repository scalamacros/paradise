package org.scalamacros.paradise
package reflect

trait StdNames {
  self: Enrichments =>
  import global._

  implicit class ParadiseNme(nme: global.nme.type) {
    val annottees          = TermName("annottees")
    val macroTransform     = TermName("macroTransform")
  }
}
