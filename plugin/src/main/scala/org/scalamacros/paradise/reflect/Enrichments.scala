package org.scalamacros.paradise
package reflect

import scala.language.implicitConversions
import scala.tools.nsc.{Global => NscGlobal}
import scala.tools.nsc.{Settings => NscSettings}
import org.scalamacros.paradise.{Settings => ParadiseSettings}

trait Enrichments extends Definitions
                     with StdNames
                     with TreeInfo
                     with StdAttachments
                     with ReflectionUtils
                     with Mirrors
                     with Symbols {

  val global: NscGlobal
  implicit def paradiseSettings(settings: NscSettings) = ParadiseSettings
  def installationFailure() = global.abort("failed to install macro paradise plugin")
}
