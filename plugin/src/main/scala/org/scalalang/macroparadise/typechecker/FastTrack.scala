package org.scalalang.macroparadise
package typechecker

trait FastTrack {
  self: Analyzer =>

  import global._
  import paradiseDefinitions._

  def updateFastTrack(sym: Symbol, expander: (MacroContext, Tree) => Tree): Unit = {
    val fastTrackField = self.getClass.getDeclaredField("fastTrack")
    fastTrackField.setAccessible(true)
    val fastTrack = fastTrackField.get(self).asInstanceOf[Map[Symbol, FastTrackEntry]]
    val fastTrackExpander: FastTrackExpander = { case (c, tree) => expander(c, tree) }
    fastTrackField.set(self, fastTrack + ((sym, FastTrackEntry(sym, fastTrackExpander))))
  }

  implicit class ParadiseFastTrack(fastTrack: self.fastTrack.type) {
    def hijack() = {
      for (m <- QuasiquoteMacros) {
        import org.scalalang.macroparadise.quasiquotes.Quasiquotes
        updateFastTrack(m, (c0, _) => (new { val c: c0.type = c0 } with Quasiquotes).expandQuasiquote)
      }
    }
  }
}
