package org.scalamacros.paradise
package reflect

trait Trees {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  def duplicateAndKeepPositions(tree: Tree) = {
    // global.duplicator won't work, because it focuses range positions
    val duplicator = new Transformer { override val treeCopy = newStrictTreeCopier }
    duplicator.transform(tree)
  }

  lazy val compat = org.scalamacros.quasiquotes.QuasiquoteCompat[self.global.type](self.global)
}
