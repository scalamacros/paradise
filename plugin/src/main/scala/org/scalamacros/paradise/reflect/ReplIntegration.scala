package org.scalamacros.paradise
package reflect

import scala.tools.nsc.interpreter._

trait ReplIntegration {
  self: Enrichments =>
  import global._

  private def obtainField(cls: Class[_], name: String) = { val result = cls.getDeclaredField(name); result.setAccessible(true); result }
  private lazy val f_intp = obtainField(classOf[ReplReporter], "intp")
  private lazy val f_executingRequest = obtainField(classOf[IMain], "executingRequest")
  private lazy val f_handlers = obtainField(classOf[IMain#Request], "handlers")
  private lazy val f_referencedNames = obtainField(classOf[IMain#Request], "referencedNames")

  // https://github.com/scalamacros/paradise/issues/19
  // REPL computes the list of defined members based on parsed code, not typechecked code
  // therefore we need to update its internal structures if that list changes
  // there's no API for that, but luckily it ended up being possible
  def tellReplAboutExpansion(sym: Symbol, companion: Symbol, expanded: List[Tree]): Unit = {
    if (global.reporter.isInstanceOf[ReplReporter] && sym.owner.name.toString == nme.INTERPRETER_IMPORT_WRAPPER.toString) {
      val intp = f_intp.get(global.reporter).asInstanceOf[IMain]
      val req = f_executingRequest.get(intp).asInstanceOf[intp.Request]
      import intp._
      import memberHandlers._
      val handlers1 = req.handlers.flatMap {
        // TODO: I challenge you to write this without a cast :)
        case dh: MemberDefHandler if dh.member.name == sym.name => expanded map (memberHandlers chooseHandler _.asInstanceOf[intp.global.Tree])
        case dh: MemberDefHandler if dh.member.name == companion.name => Nil
        case h => List(h)
      }
      val referencedNames1 = handlers1 flatMap (_.referencedNames)
      f_handlers.set(req, handlers1)
      f_referencedNames.set(req, referencedNames1)
    }
  }
}
