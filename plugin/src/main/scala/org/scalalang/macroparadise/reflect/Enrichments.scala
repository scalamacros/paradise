package org.scalalang.macroparadise
package reflect

import scala.tools.nsc.{Global => NscGlobal}

trait Enrichments extends Definitions
                     with StdNames {

  val global: NscGlobal
  import global._
  import scala.reflect.internal.Flags._

  implicit class ParadiseSymbol(sym: Symbol) {
    def isAnnotationMacro = sym.isTermMacro && sym.owner.isMacroAnnotation && sym.name == paradiseNme.macroTransform
    def isMacroAnnotation = sym.isClass && sym.hasFlag(MACRO)
  }
}
