package org.scalalang.macroparadise
package reflect

trait Symbols {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  implicit class ParadiseSymbol(sym: Symbol) {
    def isAnnotationMacro = sym.isTermMacro && sym.owner.isMacroAnnotation && sym.name == nme.macroTransform
    def isMacroAnnotation = sym.isClass && sym.hasFlag(MACRO)
  }
}
