package org.scalamacros.paradise
package reflect

trait TreeGen {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  implicit class ParadiseTreeGen(gen: global.gen.type) {
    def mkSyntheticParam(pname: TermName) =
      ValDef(Modifiers(PARAM | SYNTHETIC), pname, TypeTree(), EmptyTree)
  }
}
