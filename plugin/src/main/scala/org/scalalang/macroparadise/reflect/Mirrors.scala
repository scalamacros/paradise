package org.scalalang.macroparadise
package reflect

trait Mirrors {
  self: Enrichments =>

  import global._
  import scala.language.reflectiveCalls

  implicit class ParadiseMirror(m: RootsBase) {
    def missingHook(owner: Symbol, name: Name): Symbol = {
      val reflectiveThis = m.asInstanceOf[{ def missingHook(owner: Symbol, name: Name): Symbol }]
      reflectiveThis.missingHook(owner, name)
    }
  }
}
