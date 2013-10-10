package org.scalalang.macroparadise
package typechecker

import scala.tools.nsc.typechecker.{Implicits => NscImplicits}
import scala.reflect.internal.util.Statistics

trait Implicits extends NscImplicits {
  self: Analyzer =>

  import global._
  import definitions._
  import typeDebug.{ ptTree, ptBlock, ptLine }
  import global.typer.{ printTyping, deindentTyping, indentTyping, printInference }
  import scala.tools.nsc.typechecker.ImplicitsStats._

  override def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean, pos: Position): SearchResult = {
    printInference("[infer %s] %s with pt=%s in %s".format(
      if (isView) "view" else "implicit",
      tree, pt, context.owner.enclClass)
    )
    printTyping(
      ptBlock("infer implicit" + (if (isView) " view" else ""),
        "tree"        -> tree,
        "pt"          -> pt,
        "undetparams" -> context.outer.undetparams
      )
    )
    indentTyping()

    val rawTypeStart    = if (Statistics.canEnable) Statistics.startCounter(rawTypeImpl) else null
    val findMemberStart = if (Statistics.canEnable) Statistics.startCounter(findMemberImpl) else null
    val subtypeStart    = if (Statistics.canEnable) Statistics.startCounter(subtypeImpl) else null
    val start           = if (Statistics.canEnable) Statistics.startTimer(implicitNanos) else null
    if (printInfers && !tree.isEmpty && !context.undetparams.isEmpty)
      printTyping("typing implicit: %s %s".format(tree, context.undetparamsString))
    val implicitSearchContext = context.makeImplicit(reportAmbiguous)
    val result = new ParadiseImplicitSearch(tree, pt, isView, implicitSearchContext, pos).bestImplicit
    if ((result.isFailure || !settings.Xdivergence211.value) && saveAmbiguousDivergent && implicitSearchContext.hasErrors) {
      context.updateBuffer(implicitSearchContext.errBuffer.filter(err => err.kind == ErrorKinds.Ambiguous || err.kind == ErrorKinds.Divergent))
      debugwarn("update buffer: " + implicitSearchContext.errBuffer)
    }
    printInference("[infer implicit] inferred " + result)
    context.undetparams = context.undetparams filterNot result.subst.from.contains

    if (Statistics.canEnable) Statistics.stopTimer(implicitNanos, start)
    if (Statistics.canEnable) Statistics.stopCounter(rawTypeImpl, rawTypeStart)
    if (Statistics.canEnable) Statistics.stopCounter(findMemberImpl, findMemberStart)
    if (Statistics.canEnable) Statistics.stopCounter(subtypeImpl, subtypeStart)
    deindentTyping()
    printTyping("Implicit search yielded: "+ result)
    result
  }

  class ParadiseImplicitSearch(tree: Tree, pt: Type, isView: Boolean, context0: Context, pos0: Position)
  extends ImplicitSearch(tree, pt, isView, context0, pos0) with ParadiseTyper
}