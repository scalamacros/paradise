package org.scalamacros.paradise
package reflect

trait Names {
  self: Enrichments =>

  import global._

  object TermName {
    def apply(s: String): TermName = newTermName(s)
    def unapply(name: TermName): Some[String] = Some(name.toString)
  }

  object TypeName {
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(name: TypeName): Some[String] = Some(name.toString)
  }
}
