package org.scalalang.macroparadise
package reflect

import scala.language.implicitConversions
import scala.tools.nsc.{Global => NscGlobal}
import scala.tools.nsc.{Settings => NscSettings}
import org.scalalang.macroparadise.{Settings => ParadiseSettings}

trait Enrichments extends Definitions
                     with StdNames
                     with TreeInfo
                     with StdAttachments
                     with Trees
                     with ReflectionUtils
                     with Mirrors
                     with Symbols {

  val global: NscGlobal
  implicit def paradiseSettings(settings: NscSettings) = ParadiseSettings
  def installationFailure() = global.abort("failed to install macro paradise plugin")
}
