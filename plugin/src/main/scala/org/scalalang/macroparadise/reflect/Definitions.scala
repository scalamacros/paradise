package org.scalalang.macroparadise
package reflect

trait Definitions {
  self: Enrichments =>

  import global._
  import rootMirror._

  object paradiseDefinitions {
    lazy val InheritedAttr = requiredClass[java.lang.annotation.Inherited]
  }
}
