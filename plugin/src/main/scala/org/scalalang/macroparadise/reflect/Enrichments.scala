package org.scalalang.macroparadise
package reflect

import scala.tools.nsc.{Global => NscGlobal}

trait Enrichments extends Definitions
                     with StdNames
                     with TreeInfo
                     with StdAttachments
                     with Trees
                     with ReflectionUtils
                     with Mirrors
                     with Symbols {

  val global: NscGlobal
  def installationFailure() = global.abort("failed to install macro paradise plugin")
}
