package org.scalamacros.paradise
package reflect

import scala.reflect.internal.Flags._
import scala.reflect.internal.MissingRequirementError

trait Definitions {
  self: Enrichments =>

  import global._
  import rootMirror._
  import definitions._

  object paradiseDefinitions {
    lazy val InheritedAttr = requiredClass[java.lang.annotation.Inherited]
  }
}
