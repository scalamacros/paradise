package org.scalamacros.paradise
package reflect

trait StdAttachments {
  self: Enrichments =>
  import global._
  import scala.collection.{immutable, mutable}

  case object WeakSymbolAttachment
  def markWeak(sym: Symbol) = if (sym != null && sym != NoSymbol) sym.updateAttachment(WeakSymbolAttachment) else sym
  def unmarkWeak(sym: Symbol) = if (sym != null && sym != NoSymbol) sym.removeAttachment[WeakSymbolAttachment.type] else sym
  def isWeak(sym: Symbol) = sym == null || sym == NoSymbol || sym.attachments.get[WeakSymbolAttachment.type].isDefined

  case class SymbolCompleterAttachment(info: Type)
  def backupCompleter(sym: Symbol): Symbol = {
    if (sym != null && sym != NoSymbol) {
      assert(sym.rawInfo.isInstanceOf[LazyType], s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with ${sym.rawInfo.kind}")
      sym.updateAttachment(SymbolCompleterAttachment(sym.rawInfo))
    } else sym
  }
  def restoreCompleter(sym: Symbol): Unit = {
    if (sym != null && sym != NoSymbol) {
      val oldCompleter = sym.attachments.get[SymbolCompleterAttachment].get.info
      sym setInfo oldCompleter
      sym.attachments.remove[SymbolCompleterAttachment]
    } else ()
  }

  // here we should really store and retrieve duplicates of trees in order to avoid leakage through tree attributes
  case class SymbolSourceAttachment(source: Tree)
  def attachSource(sym: Symbol, tree: Tree): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(SymbolSourceAttachment(tree.duplicate)) else sym
  def attachedSource(sym: Symbol): Tree = if (sym != null && sym != NoSymbol) sym.attachments.get[SymbolSourceAttachment].map(_.source.duplicate).getOrElse(EmptyTree) else EmptyTree

  // unfortunately we cannot duplicate here, because that would dissociate the symbol from its derived symbols
  // that's because attachExpansion(tree) happens prior to enterSym(tree), so if we duplicate the assigned symbol never makes it into the att
  // in its turn, that would mean that we won't be able to handle recursive expansions in typedTemplate
  // because by the time typedTemplate gets activated, everything's already expanded by templateSig
  // so we need to go from original trees/symbols to recursively expanded ones and that requires links to derived symbols
  // TODO: should be a better solution
  case class SymbolExpansionAttachment(expansion: List[Tree])
  def hasAttachedExpansion(sym: Symbol) = sym.attachments.get[SymbolExpansionAttachment].isDefined
  def attachExpansion(sym: Symbol, trees: List[Tree]): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(SymbolExpansionAttachment(trees/*.map(_.duplicate)*/)) else sym
  def attachedExpansion(sym: Symbol): Option[List[Tree]] = if (sym != null && sym != NoSymbol) sym.attachments.get[SymbolExpansionAttachment].map(_.expansion/*.map(_.duplicate)*/) else None

  import SymbolExpansionStatus._
  private def checkExpansionStatus(sym: Symbol, p: SymbolExpansionStatus => Boolean) = sym.attachments.get[SymbolExpansionStatus].map(p).getOrElse(false)
  def isMaybeExpandee(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isUnknown)
  def isExpanded(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isExpanded)
  def isNotExpandable(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isNotExpandable)
  def markMaybeExpandee(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(Unknown) else sym
  def markExpanded(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(Expanded) else sym
  def markNotExpandable(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(NotExpandable) else sym
  def unmarkExpanded(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.removeAttachment[SymbolExpansionStatus] else sym

  case class CacheAttachment(cache: mutable.Map[String, Any])
  implicit class RichTree(tree: Tree) {
    def cached[T](key: String, op: => T): T = {
      val cache = tree.attachments.get[CacheAttachment].map(_.cache).getOrElse(mutable.Map[String, Any]())
      val result = cache.getOrElseUpdate(key, op).asInstanceOf[T]
      tree.updateAttachment(CacheAttachment(cache))
      result
    }
  }
}

class SymbolExpansionStatus private (val value: Int) extends AnyVal {
  def isUnknown = this == SymbolExpansionStatus.Unknown
  def isExpanded = this == SymbolExpansionStatus.Expanded
  def isNotExpandable = this == SymbolExpansionStatus.NotExpandable
}
object SymbolExpansionStatus {
  val Unknown = new SymbolExpansionStatus(0)
  val Expanded = new SymbolExpansionStatus(1)
  val NotExpandable = new SymbolExpansionStatus(2)
}
