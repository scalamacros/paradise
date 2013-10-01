package org.scalalang.macroparadise
package reflect

import scala.reflect.api.QuasiquoteCompatV2

trait Trees {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  def duplicateAndKeepPositions(tree: Tree) = {
    // global.duplicator won't work, because it focuses range positions
    val duplicator = new Transformer { override val treeCopy = newStrictTreeCopier }
    duplicator.transform(tree)
  }

  lazy val compat = QuasiquoteCompatV2(self.global)
}
