package org.scalalang.macroparadise
package reflect

trait Types {
  self: Enrichments =>

  import global._

  def isImplicitMethodType(tp: Type) = tp match {
    case mt: MethodType => mt.isImplicit
    case _              => false
  }
}
