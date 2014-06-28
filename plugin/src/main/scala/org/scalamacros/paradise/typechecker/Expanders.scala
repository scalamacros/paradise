package org.scalamacros.paradise
package typechecker

trait Expanders {
  self: AnalyzerPlugins =>

  import global._
  import analyzer._
  import definitions._
  import scala.reflect.internal.Flags._
  import scala.reflect.internal.Mode._
  import analyzer.{Namer => NscNamer}

  def mkExpander(namer0: NscNamer) = new { val namer: NscNamer = namer0 } with Namer with Expander
  trait Expander {
    self: Namer with Expander =>

    val namer: NscNamer
    import namer._
    val expanderErrorGen = new ErrorGen(namer.typer)
    import expanderErrorGen._

    def prepareAnnotationMacro(ann: Tree, mann: Symbol, sym: Symbol, annottee: Tree, expandee: Tree): Tree = {
      val companion = if (expandee.isInstanceOf[ClassDef]) patchedCompanionSymbolOf(sym, context) else NoSymbol
      val companionSource = if (!isWeak(companion)) attachedSource(companion) else EmptyTree
      val expandees = List(annottee, expandee, companionSource).distinct.filterNot(_.isEmpty)
      val safeExpandees = expandees.map(expandee => duplicateAndKeepPositions(expandee)).map(_.setSymbol(NoSymbol))
      val prefix = Select(ann, nme.macroTransform) setSymbol mann.info.member(nme.macroTransform) setPos ann.pos
      Apply(prefix, safeExpandees) setPos ann.pos
    }

    def expandAnnotationMacro(original: Tree, expandee: Tree): Option[List[Tree]] = {
      val sym = original.symbol
      val companion = if (original.isInstanceOf[ClassDef]) patchedCompanionSymbolOf(sym, context) else NoSymbol
      val wasWeak = isWeak(companion)
      val wasTransient = companion == NoSymbol || companion.isSynthetic
      def rollThroughImports(context: Context): Context = {
        if (context.isInstanceOf[ImportContext]) rollThroughImports(context.outer)
        else context
      }
      val typer = {
        // expanding at top level => allow the macro to see everything
        if (sym.isTopLevel) newTyper(context)
        // expanding at template level => only allow to see outside of the enclosing class
        // we have to skip two contexts:
        //  1) the Template context that hosts members
        //  2) the ImplDef context that hosts type params (and just them?)
        // upd. actually, i don't think we should skip the second context
        // that doesn't buy us absolutely anything wrt robustness
        else if (sym.owner.isClass) newTyper(rollThroughImports(context).outer)
        // expanding at block level => only allow to see outside of the block
        else newTyper(rollThroughImports(context).outer)
      }
      def expand() = (new DefMacroExpander(typer, expandee, NOmode, WildcardType) {
        override def onSuccess(expanded: Tree) = expanded
      })(expandee) match {
        case tree if tree.isErroneous => None
        case tree => Some(tree)
      }
      def extract(expanded: Tree): List[Tree] = expanded match {
        case Block(stats, Literal(Constant(()))) => stats // ugh
        case tree => List(tree)
      }
      def validate(expanded: List[Tree]): Option[List[Tree]] = {
        if (sym.owner.isPackageClass) {
          original match {
            case ClassDef(_, originalName, _, _) =>
              expanded match {
                case (expandedClass @ ClassDef(_, className, _, _)) :: Nil
                if className == originalName && wasWeak =>
                  attachExpansion(sym, List(expandedClass))
                  attachExpansion(companion, Nil)
                  Some(expanded)
                case (expandedCompanion @ ModuleDef(_, moduleName, _)) :: (expandedClass @ ClassDef(_, className, _, _)) :: Nil
                if className == originalName && moduleName == originalName.toTermName =>
                  attachExpansion(sym, if (wasWeak) List(expandedClass, expandedCompanion) else List(expandedClass))
                  attachExpansion(companion, List(expandedCompanion))
                  Some(expanded)
                case (expandedClass @ ClassDef(_, className, _, _)) :: (expandedCompanion @ ModuleDef(_, moduleName, _)) :: Nil
                if className == originalName && moduleName == originalName.toTermName =>
                  attachExpansion(sym, if (wasWeak) List(expandedClass, expandedCompanion) else List(expandedClass))
                  attachExpansion(companion, List(expandedCompanion))
                  Some(expanded)
                case _ =>
                  if (wasWeak) MacroAnnotationTopLevelClassWithoutCompanionBadExpansion(expandee)
                  else MacroAnnotationTopLevelClassWithCompanionBadExpansion(expandee)
                  None
              }
            case ModuleDef(_, originalName, _) =>
              expanded match {
                case (expandedModule @ ModuleDef(_, expandedName, _)) :: Nil if expandedName == originalName =>
                  attachExpansion(sym, List(expandedModule))
                  Some(expanded)
                case _ =>
                  MacroAnnotationTopLevelModuleBadExpansion(expandee)
                  None
              }
          }
        } else {
          if (wasTransient) {
            attachExpansion(sym, expanded)
            attachExpansion(companion, Nil)
          } else {
            def companionRelated(tree: Tree) = tree.isInstanceOf[ModuleDef] && tree.asInstanceOf[ModuleDef].name == companion.name
            val (forCompanion, forSym) = expanded.partition(companionRelated)
            attachExpansion(sym, forSym)
            attachExpansion(companion, forCompanion)
          }
          Some(expanded)
        }
      }
      for {
        lowlevelExpansion <- expand()
        expansion <- Some(extract(lowlevelExpansion))
        duplicated = expansion.map(duplicateAndKeepPositions)
        validatedExpansion <- validate(duplicated)
      } yield validatedExpansion
    }

    def expandMacroAnnotations(stats: List[Tree]): List[Tree] = {
      def mightNeedTransform(stat: Tree) = stat match {
        case stat: MemberDef => isMaybeExpandee(stat.symbol) || hasAttachedExpansion(stat.symbol)
        case _ => false
      }
      if (phase.id > currentRun.typerPhase.id || !stats.exists(mightNeedTransform)) stats
      else stats.flatMap(stat => {
        if (mightNeedTransform(stat)) {
          val sym = stat.symbol
          assert(sym != NoSymbol, (sym, stat))
          if (isMaybeExpandee(sym)) {
            def assert(what: Boolean) = Predef.assert(what, s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with ${sym.rawInfo.kind}")
            assert(sym.rawInfo.isInstanceOf[Namer#MaybeExpandeeCompleter])
            sym.rawInfo.completeOnlyExpansions(sym)
            assert(!sym.rawInfo.isInstanceOf[Namer#MaybeExpandeeCompleter])
          }
          val derivedTrees = attachedExpansion(sym).getOrElse(List(stat))
          val (me, others) = derivedTrees.partition(_.symbol == sym)
          me ++ expandMacroAnnotations(others)
        } else {
          List(stat)
        }
      })
    }
  }
}
