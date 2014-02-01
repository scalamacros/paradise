package org.scalamacros.paradise
package typechecker

trait Namers {
  self: Analyzer =>

  import global._
  import definitions._
  import scala.reflect.internal.Flags._
  import scala.language.reflectiveCalls

  trait ParadiseNamer extends Namer with ParadiseNamerContextErrors {
    import NamerErrorGen._
    import ParadiseNamerErrorGen._

    override def enterSym(tree: Tree): Context = {
      def dispatch() = {
        var returnContext = this.context
        tree match {
          case DocDef(_, mdef) =>
            enterSym(mdef)
          case tree @ Import(_, _) =>
            createAssignAndEnterSymbol(tree)
            finishSymbol(tree)
            returnContext = context.make(tree)
          case tree: MemberDef =>
            createAssignAndEnterSymbol(tree)
            finishSymbol(tree)
          case _ =>
        }
        returnContext
      }
      tree.symbol match {
        case NoSymbol => try dispatch() catch typeErrorHandler(tree, this.context)
        case sym      => enterExistingSym(sym)
      }
    }

    def createAssignAndEnterSymbol(tree: Tree, mask: Long = -1L): Symbol = {
      def coreCreateAssignAndEnterSymbol = {
        val sym = tree match {
          case PackageDef(pid, _) => createPackageSymbol(tree.pos, pid) // package symbols are entered elsewhere
          case Import(_, _)       => createImportSymbol(tree) // import symbols are dummies, no need to enter them anywhere
          case mdef: MemberDef    => enterInScope(setPrivateWithin(mdef, createMemberSymbol(mdef, mdef.name, mask)))
          case _                  => abort("Unexpected tree: " + tree)
        }
        sym.name.toTermName match {
          case nme.IMPORT | nme.OUTER | nme.ANON_CLASS_NAME | nme.ANON_FUN_NAME | nme.CONSTRUCTOR => ()
          case _                                                                                  =>
            log("[+symbol] " + sym.debugLocationString)
        }
        tree.symbol = sym
        sym
      }
      def deriveSymbolFromSource(tree: Tree)(pf: PartialFunction[Tree, Symbol]): Symbol = {
        val sym = pf(tree)
        // can't do this in coreCreateAssignAndEnterSymbol
        // because then we won't get to update sources for redefinitions
        // this might be crucial when we have classfiles of the definition we're currently compiling
        attachSource(sym, tree)
        sym
      }
      deriveSymbolFromSource(tree) {
        case tree @ ClassDef(mods, name, _, _) =>
          val existing = context.scope.lookup(name)
          val isRedefinition = (
               existing.isType
            && existing.isTopLevel
            && context.scope == existing.owner.info.decls
            && (
                 currentRun.canRedefine(existing) ||
                 isExpanded(existing)
               )
          )
          val clazz: Symbol = {
            if (isRedefinition) {
              updatePosFlags(existing, tree.pos, mods.flags)
              setPrivateWithin(tree, existing)
              tree.symbol = existing
              existing
            }
            else coreCreateAssignAndEnterSymbol setFlag inConstructorFlag
          }
          if (clazz.isClass && clazz.isTopLevel) {
            if (clazz.sourceFile != null && clazz.sourceFile != contextFile)
              devWarning(s"Source file mismatch in $clazz: ${clazz.sourceFile} vs. $contextFile")

            clazz.associatedFile = contextFile
            if (clazz.sourceFile != null) {
              assert(currentRun.canRedefine(clazz) || clazz.sourceFile == currentRun.symSource(clazz), clazz.sourceFile)
              currentRun.symSource(clazz) = clazz.sourceFile
            }
            registerTopLevelSym(clazz)
            assert(clazz.name.toString.indexOf('(') < 0, clazz.name)  // )
          }
          clazz
        case tree @ ModuleDef(mods, name, _) =>
          var m: Symbol = context.scope lookupAll name find (_.isModule) getOrElse NoSymbol
          val moduleFlags = mods.flags | MODULE
          if (m.isModule && !m.isPackage && (currentRun.canRedefine(m) || m.isSynthetic || isExpanded(m))) {
            updatePosFlags(m, tree.pos, moduleFlags)
            setPrivateWithin(tree, m)
            // why don't we update moduleClass' flags?
            m.moduleClass andAlso (setPrivateWithin(tree, _))
            context.unit.synthetics -= m
            tree.symbol = m
          }
          else {
            m = coreCreateAssignAndEnterSymbol
            m.moduleClass setFlag moduleClassFlags(moduleFlags)
            setPrivateWithin(tree, m.moduleClass)
          }
          m.moduleClass setInfo namerOf(m).moduleClassTypeCompleter(tree)
          if (m.isTopLevel && !m.isPackage) {
            m.moduleClass.associatedFile = contextFile
            currentRun.symSource(m) = m.moduleClass.sourceFile
            registerTopLevelSym(m)
          }
          m
        case _ =>
          coreCreateAssignAndEnterSymbol
      }
    }

    def finishSymbol(tree: Tree) {
      // annotations on parameters expand together with their owners
      // therefore when we actually get to enter the parameters, we shouldn't even bother checking
      // TODO: we don't handle primary ctors that might get spuriously marked as maybe expandees because of primary paramss
      val aprioriNotExpandable = (context.tree, tree) match {
        case (ClassDef(_, _, _, _), TypeDef(_, _, _, _)) => true
        case (Template(_, _, _), ValDef(mods, _, _, _)) if mods.isParamAccessor => true
        // vparamss of primary ctors are entered in `enterValueParams`, which doesn't call us
        case (DefDef(_, _, _, _, _, _), TypeDef(_, _, _, _)) => true
        // vparamss of normal methods are also entered in `enterValueParams`, which doesn't call us
        case (TypeDef(_, _, _, _), TypeDef(_, _, _, _)) => true
        case _ => false
      }

      if (aprioriNotExpandable) finishSymbolNotExpandee(tree)
      else {
        treeInfo.getAnnotationZippers(tree) match {
          case Nil => finishSymbolNotExpandee(tree)
          case zippers => finishSymbolMaybeExpandee(tree, zippers)
        }

        // this will only show companions defined above ourselves
        // so when finishing `class C` in `{ class C; object C }`
        // we won't see `object C` in `companion` - we will see NoSymbol
        // that's the limitation of how namer works, but nevertheless it's not a problem for us
        // because if finishing `class C` doesn't set up the things, finishing `object C` will
        val sym = tree.symbol
        val companion = companionSymbolOf(sym, context)

        tree match {
          // TODO: should we also support annotations on modules expanding companion classes?
          case tree @ ClassDef(_, _, _, _) if isMaybeExpandee(sym) =>
            val wasExpanded = isExpanded(companion)
            val m = weakEnsureCompanionObject(tree)
            finishSymbolMaybeExpandeeCompanion(attachedSource(m), m, sym)
            if (wasExpanded) markExpanded(m) // why is this necessary? see files/run/macro-annotation-recursive-class
                                             // TODO: in general, this first call to FSMEC usually only brings grief
                                             // can we get rid of it completely without having to sweep its results under the carpet?
          case tree @ ModuleDef(_, _, _) if isMaybeExpandee(companion) =>
            finishSymbolMaybeExpandeeCompanion(tree, sym, companion)
          case _ =>
        }
      }
    }

    def finishSymbolNotExpandee(tree: Tree) {
      val sym = tree.symbol
      tree match {
        case tree @ PackageDef(_, _) =>
          newNamer(context.make(tree, sym.moduleClass, sym.info.decls)) enterSyms tree.stats
        case tree @ ClassDef(mods, name, tparams, impl) =>
          sym setInfo completerOf(tree)
          if (mods.isCase) {
            val m = ensureCompanionObject(tree, caseModuleDef)
            m.moduleClass.updateAttachment(new ClassForCaseCompanionAttachment(tree))
          }
          if (treeInfo.anyConstructorHasDefault(tree)) {
            val m = ensureCompanionObject(tree)
            m.updateAttachment(new ConstructorDefaultsAttachment(tree, null))
          }
          val owner = tree.symbol.owner
          if (settings.lint && owner.isPackageObjectClass && !mods.isImplicit) {
            context.unit.warning(tree.pos,
              "it is not recommended to define classes/objects inside of package objects.\n" +
              "If possible, define " + tree.symbol + " in " + owner.skipPackageObject + " instead."
            )
          }
          // Suggested location only.
          if (mods.isImplicit) {
            if (treeInfo.primaryConstructorArity(tree) == 1) {
              log("enter implicit wrapper "+tree+", owner = "+owner)
              enterImplicitWrapper(tree)
            }
            else MultipleParametersImplicitClassError(tree)
          }
          validateCompanionDefs(tree)
        case tree @ ModuleDef(_, _, _) =>
          unmarkWeak(sym)
          sym setInfo completerOf(tree)
          validateCompanionDefs(tree)
        case tree @ ValDef(_, _, _, _) =>
          if (noEnterGetterSetter(tree)) {
            tree.symbol setInfo completerOf(tree)
          } else {
            // when refactoring enterSym, I needed to decouple symbol creation and various syntheses
            // so that annotation expansion mechanism could be installed in-between of those
            // it went well except for one thing - ValDef symbol creation is very closely tied to syntheses
            // because depending on whether the ValDef is a val, var or a lazy val, different symbols need to be generated
            // since I didn't have much time (and, back then, much understanding), I just decided to create dummies
            // that live only to stand in as potential annottees and get destroyed if any sort of synthesis is necessary
            // TODO: this is obviously ugly and needs to be fixed
            context.scope.unlink(tree.symbol)
            tree.symbol setInfo NoType
            enterGetterSetter(tree)
          }
          // When java enums are read from bytecode, they are known to have
          // constant types by the jvm flag and assigned accordingly.  When
          // they are read from source, the java parser marks them with the
          // STABLE flag, and now we receive that signal.
          if (tree.symbol hasAllFlags STABLE | JAVA)
            tree.symbol setInfo ConstantType(Constant(tree.symbol))
        case tree @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
          sym setInfo completerOf(tree)
        case tree @ DefDef(mods, name, tparams, _, _, _) =>
          val bridgeFlag = if (mods hasAnnotationNamed tpnme.bridgeAnnot) BRIDGE | ARTIFACT else 0
          sym setFlag bridgeFlag
          if (name == nme.copy && sym.isSynthetic) enterCopyMethod(tree)
          else sym setInfo completerOf(tree)
        case tree @ TypeDef(_, _, _, _) =>
          sym setInfo completerOf(tree)
        case tree @ Import(_, _) =>
          sym setInfo completerOf(tree)
      }
    }

    // we have several occasions when so called "maybe expandees" need special care
    // ("maybe expandees" = annotated members, which might or might not be annotated with a macro expansion)
    // 1) (when called by Symbol.info) trigger the MaybeExpandeeCompleter and then immediately recur into a fresh completer
    //    if we don't recur, we're doomed to fail, because there are only so many retries that Symbol.info can tolerate
    //    and this retry threshold is already fine-tuned to the current chain of completers, which makes MaybeExpandeeCompleter one too many
    // 2) (when called by expandMacroAnnotations from templateSig or typedBlock) in this situation noone needs us to fully complete
    //    the underlying symbol. just making sure that we don't have any annotations to expand is the least and the most we should do.
    //    if we're overeager like in mode #1, we might easily induce cyclic reference errors (like in tests/run/macro-annotations-packageobject)
    // 3) (when called by Symbol.typeParams) this one is different from Symbol.info, because it calls load, not complete
    //    from what I understand, this separation exists because it takes much less effort to figure out tparams rather than the full signature
    //    for example, vanilla completers assigned in namer are created with typeParams already known
    //    you can see for yourself in the distinction between monoTypeCompleter and PolyTypeCompleter
    //    therefore, just as with Symbol.info we need to trigger the MaybeExpandeeCompleter
    //    and then not forget to recur into the fresh completer's load, again because of the retry limit baked into Symbol.typeParams
    // 4) TODO: (when called by Symbol.unsafeTypeParams) figure out what's the deal with them
    //    existence of this method profoundly scares me, even though I never had a problem with it
    abstract class MaybeExpandeeCompleter(val tree: Tree) extends LockingTypeCompleter with FlagAssigningCompleter {
      def destroy(syms: Symbol*) = {
        for (sym <- syms) {
          context.unit.synthetics -= sym
          context.scope.unlink(sym)
          sym setInfo NoType
          sym.moduleClass setInfo NoType
          sym.removeAttachment[SymbolCompleterAttachment]
        }
      }

      def complete(sym: Symbol, onlyExpansions: Boolean) = {
        val reflectiveThis = ParadiseNamer.this.asInstanceOf[{ var _lockedCount: Int }]
        reflectiveThis._lockedCount += 1
        try completeImpl(sym, onlyExpansions)
        finally reflectiveThis._lockedCount -= 1
      }

      override def completeImpl(sym: Symbol): Unit = {
        completeImpl(sym, onlyExpansions = false)
      }

      def completeImpl(sym: Symbol, onlyExpansions: Boolean): Unit = {
        val thisCompleter = sym.rawInfo
        maybeExpand()
        assert(sym.rawInfo != thisCompleter, s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with $kind")
        if (onlyExpansions) sym.rawInfo.completeOnlyExpansions(sym)
        else sym.rawInfo.complete(sym)
      }

      override def load(sym: Symbol): Unit = {
        this.completeOnlyExpansions(sym)
        sym.rawInfo.load(sym)
      }

      def maybeExpand(): Unit // TODO: should I also pass `sym` here?
    }

    abstract class MaybeExpandeeCompanionCompleter(tree: Tree) extends MaybeExpandeeCompleter(tree)

    implicit class RichType(tpe: Type) {
      def completeOnlyExpansions(sym: Symbol) = tpe match {
        case mec: ParadiseNamer#MaybeExpandeeCompleter => mec.complete(sym, onlyExpansions = true)
        case c => ()
      }
    }

    def finishSymbolMaybeExpandee(tree: Tree, annZippers: List[AnnotationZipper]) {
      val sym = tree.symbol
      unmarkWeak(sym)
      markMaybeExpandee(sym)
      sym.setInfo(new MaybeExpandeeCompleter(tree) {
        override def kind = s"maybeExpandeeCompleter for ${sym.accurateKindString} ${sym.rawname}#${sym.id}"
        override def maybeExpand(): Unit = {
          val companion = if (tree.isInstanceOf[ClassDef]) companionSymbolOf(sym, context) else NoSymbol

          def maybeExpand(annotation: Tree, annottee: Tree, maybeExpandee: Tree): Option[List[Tree]] = {
            val treeInfo.Applied(Select(New(tpt), nme.CONSTRUCTOR), _, _) = annotation
            val mann = probeMacroAnnotation(context, tpt)
            if (mann.isMacroAnnotation) {
              assert(!currentRun.compiles(mann), mann)
              val annm = prepareAnnotationMacro(annotation, mann, sym, annottee, maybeExpandee)
              expandAnnotationMacro(tree, annm)
              // if we encounter an error, we just return None, so that other macro annotations can proceed
              // this is unlike macroExpand1 when any error in an expandee blocks expansions
              // there it's necessary in order not to exacerbate typer errors
              // but when manning we aren't in typer, so we don't have to do as macroExpand1 does
              // and also there's a good reason not to ban other macro annotations
              // if we do ban them, we might get spurious compilation errors from non-existent members that could've been generated
            } else {
              None
            }
          }

          annZippers.toStream.flatMap(annz => maybeExpand(annz.annotation, annz.annottee, annz.owner)).headOption match {
            case Some(expanded) =>
              markExpanded(sym)
              markExpanded(companion)
              // expansion brings new trees, probably wildly different from current ones. what do we do?
              // the most robust thing would be to destroy ourselves (us and our companion), but we can't do that at top level
              // therefore at top level we don't destroy, but rather rely on enterSyms to redefine ourselves
              // however when nested we go all out
              // TODO: unlinking distorts the order of symbols in scope
              // note however that trees (calculated by expandMacroAnnotations) will be generated in correct order
              if (!sym.isTopLevel) destroy(sym, companion)
              enterSyms(expanded) // TODO: we can't reliably expand into imports, because they won't be accounted by definitions below us
            case None =>
              markNotExpandable(sym)
              finishSymbolNotExpandee(tree)
          }

          // take care of the companion if it's no longer needed
          // we can't do this in companion's completer, because that one isn't guaranteed to ever be called
          val expandedWithoutCompanion = isExpanded(sym) && attachedExpansion(companion).map(_.isEmpty).getOrElse(false)
          val companionHasReemerged = expandedWithoutCompanion && sym.isTopLevel && !isWeak(companion)
          val notExpandableWeakCompanion = isNotExpandable(sym) && isWeak(companion)
          if ((expandedWithoutCompanion && !companionHasReemerged) || notExpandableWeakCompanion) destroy(companion)
        }
      })
    }

    // how do we make sure that this completer falls back to the vanilla completer if the companion ends up not expanding?
    // well, if a module symbol has a maybeExpandee companion then the last two calls to its setInfo will be one of:
    //   * non-FSMEC completer for the module and then FSMEC => fallback should call native completer
    //   * FSMEC from enterSyntheticSym for a phantom module and then FSMEC again => fallback should do nothing
    // now it's easy to see that both are correctly handled here
    def finishSymbolMaybeExpandeeCompanion(tree: Tree, m: Symbol, c: Symbol) {
      val worthBackingUp = !m.rawInfo.isInstanceOf[ParadiseNamer#MaybeExpandeeCompanionCompleter]
      if (worthBackingUp) backupCompleter(m)
      markMaybeExpandee(m)
      m.setInfo(new MaybeExpandeeCompanionCompleter(tree) {
        override def kind = s"maybeExpandeeCompanionCompleter for ${m.rawname}#${m.id}"
        override def maybeExpand(): Unit = {
          c.rawInfo.completeOnlyExpansions(c)
          // this is a very tricky part of annotation expansion
          // because now, after deferring to our companion's judgement for a while, we have to ourselves figure out:
          //   1) whether we should start completing on our own
          //   2) if we should do it on our own, then how exactly
          // 1 is easy. If our companion's expansion has destroyed us (or hasn't materialized us if we were weak)
          // then we no longer care and we silently go into oblivion. Otherwise, we should take care of ourselves.
          // 2 is hard, because we have two distinct situations to handle:
          //   2a) isExpanded(c) is true, which means that our companion has just expanded
          //   2b) isNotExpandable(c) is true, which means that our companion has just been deemed unexpandable
          // 2a is simple, because it means that we don't have to do anything, as we've either got destroyed
          // or we've got entered in `enterSyms(expanded)` that follows expansions.
          // 2b is tricky, because it means that we need to fall back to the most recent non-FSMEC completer.
          // The hardest part here is that we can't just get to the completer that was preceding `this` as m.rawInfo
          // (otherwise we run into issue #9, for more details see history of this change). Instead we need to track m's type history.
          val destroyedDuringExpansion = m.rawInfo == NoType
          val failedToMaterializeDuringExpansion = isWeak(m)
          val aliveAndKicking = !destroyedDuringExpansion && !failedToMaterializeDuringExpansion
          if (aliveAndKicking && isNotExpandable(c)) {
            restoreCompleter(m)
            val maybeExpandee = m.rawInfo.isInstanceOf[ParadiseNamer#MaybeExpandeeCompleter]
            if (maybeExpandee) markMaybeExpandee(m) else markNotExpandable(m)
          }
        }
      })
    }

    // mostly copy/pasted and adapted from typedIdent
    // adaptations = ignore error reporting + ignore java + don't force symbols being compiled
    // the last requirement leads to us being imprecise in some situation wrt normal name resolution
    // but that's okay, since it's the only way for manns to remain modular and not to cripple normal annotations
    def probeMacroAnnotation(context: Context, tpt: Tree): Symbol = {
      // SAFE HELPERS (can't cause unnecessary completions)
      def reallyExists(sym: Symbol) = { if (newTyper(context).isStale(sym)) sym.setInfo(NoType); exists(sym) }
      def qualifies(sym: Symbol): Boolean = sym.hasRawInfo && reallyExists(sym)

      // UNSAFE HELPERS (need to guard against unnecessary completions)
      def canDefineMann(sym: Symbol): Boolean = !currentRun.compiles(sym)
      def exists(sym: Symbol) = if (canDefineMann(sym)) sym.exists else false
      def importedSymbol(imp: ImportInfo, name: Name): Symbol = { // TODO: be more precise in reproducing importSig and importedSymbol
        val impContext = context.enclosingContextChain.find(_.tree.symbol == imp.tree.symbol).get
        val sym = probeMacroAnnotation(impContext.outer, imp.tree.expr)
        val pre = if (reallyExists(sym) && isAccessible(impContext, sym)) sym.tpe else NoType
        var result: Symbol = NoSymbol
        var renamed = false
        var selectors = imp.tree.selectors
        while (selectors != Nil && result == NoSymbol) {
          if (selectors.head.rename == name.toTermName)
            result = nonLocalMember(pre, if (name.isTypeName) selectors.head.name.toTypeName else selectors.head.name)
          else if (selectors.head.name == name.toTermName)
            renamed = true
          else if (selectors.head.name == nme.WILDCARD && !renamed)
            result = nonLocalMember(pre, name)
          selectors = selectors.tail
        }
        result
      }
      // def isAccessible(cx: Context, sym: Symbol) = if (canDefineMann(cx.owner)) cx.isAccessible(sym, cx.prefix, superAccess = false) else false
      def isAccessible(cx: Context, sym: Symbol) = true // TODO: sorry, it's 2am, and I can't figure this out
      def member(tpe: Type, name: Name) = if (canDefineMann(tpe.typeSymbol)) tpe.member(name) else NoSymbol
      def nonLocalMember(tpe: Type, name: Name) = if (canDefineMann(tpe.typeSymbol)) tpe.nonLocalMember(name) else NoSymbol

      if (tpt.hasSymbolField && tpt.symbol != NoSymbol) tpt.symbol
      else tpt match {
        case Ident(name) =>

          // STEP 1: RESOLVE THE NAME IN SCOPE
          var defSym: Symbol = NoSymbol
          var defEntry: ScopeEntry = null
          var cx = context
          while (defSym == NoSymbol && cx != NoContext && (cx.scope ne null)) {
            defEntry = cx.scope.lookupEntry(name)
            if ((defEntry ne null) && qualifies(defEntry.sym)) defSym = defEntry.sym
            else {
              cx = cx.enclClass
              val foundSym = member(cx.prefix, name) filter qualifies
              defSym = foundSym filter (isAccessible(cx, _))
              if (defSym == NoSymbol) cx = cx.outer
            }
          }

          // STEP 2: RESOLVE THE NAME IN IMPORTS
          val symDepth = if (defEntry eq null) cx.depth
                         else cx.depth - ({
                           if (cx.scope ne null) cx.scope.nestingLevel
                           else 0 // TODO: fix this in toolboxes, not hack around here
                         } - defEntry.owner.nestingLevel)
          var impSym: Symbol = NoSymbol
          var imports = context.imports
          while (!reallyExists(impSym) && !imports.isEmpty && imports.head.depth > symDepth) {
            impSym = importedSymbol(imports.head, name)
            if (!exists(impSym)) imports = imports.tail
          }

          // FIXME: repl hack. somehow imports that come from repl are doubled
          // e.g. after `import $line7.$read.$iw.$iw.foo` you'll have another identical `import $line7.$read.$iw.$iw.foo`
          // this is a crude workaround for the issue
          imports match {
            case fst :: snd :: _ if exists(impSym) && fst == snd => imports = imports.tail
            case _ => // do nothing
          }

          // STEP 3: TRY TO RESOLVE AMBIGUITIES
          if (exists(defSym) && exists(impSym)) {
            if (defSym.isDefinedInPackage &&
                (!currentRun.compiles(defSym) ||
                 context.unit.exists && defSym.sourceFile != context.unit.source.file))
              defSym = NoSymbol
            else if (impSym.isError || impSym.name == nme.CONSTRUCTOR)
              impSym = NoSymbol
          }
          if (!exists(defSym) && exists(impSym)) {
            var impSym1: Symbol = NoSymbol
            var imports1 = imports.tail
            while (!imports1.isEmpty &&
                   (!imports.head.isExplicitImport(name) ||
                    imports1.head.depth == imports.head.depth)) {
              impSym1 = importedSymbol(imports1.head, name)
              if (reallyExists(impSym1)) {
                if (imports1.head.isExplicitImport(name)) {
                  if (imports.head.isExplicitImport(name) ||
                      imports1.head.depth != imports.head.depth) return NoSymbol // was possibly fixable ambiguous import
                  impSym = impSym1
                  imports = imports1
                } else if (!imports.head.isExplicitImport(name) &&
                           imports1.head.depth == imports.head.depth) return NoSymbol // was possibly fixable ambiguous import
              }
              imports1 = imports1.tail
            }
          }

          // STEP 4: DEAL WITH WHAT WE HAVE
          if (exists(defSym) && !exists(impSym)) defSym
          else if (exists(defSym) && exists(impSym)) NoSymbol // was ambiguous import
          else if (!exists(defSym) && exists(impSym)) impSym
          else {
            val lastTry = rootMirror.missingHook(rootMirror.RootClass, name)
            if (lastTry != NoSymbol && isAccessible(context, lastTry)) lastTry
            else NoSymbol
          }
        case Select(qualtree, name) => // TODO: be more precise wrt typedSelect
          val qual = probeMacroAnnotation(context, qualtree)
          val sym = if (canDefineMann(qual)) member(qual.tpe, name) else NoSymbol
          if (reallyExists(sym) && isAccessible(context, sym)) sym else NoSymbol
        case _ =>
          NoSymbol
      }
    }

    def prepareAnnotationMacro(ann: Tree, mann: Symbol, sym: Symbol, annottee: Tree, expandee: Tree): Tree = {
      val companion = if (expandee.isInstanceOf[ClassDef]) companionSymbolOf(sym, context) else NoSymbol
      val companionSource = if (!isWeak(companion)) attachedSource(companion) else EmptyTree
      val expandees = List(annottee, expandee, companionSource).distinct.filterNot(_.isEmpty)
      val safeExpandees = expandees.map(_.duplicate).map(_.setSymbol(NoSymbol))
      val prefix = Select(ann, nme.macroTransform) setSymbol mann.info.member(nme.macroTransform) setPos ann.pos
      Apply(prefix, safeExpandees) setPos ann.pos
    }

    def expandAnnotationMacro(original: Tree, expandee: Tree): Option[List[Tree]] = {
      val sym = original.symbol
      val companion = if (original.isInstanceOf[ClassDef]) companionSymbolOf(sym, context) else NoSymbol
      val wasWeak = isWeak(companion)
      val wasTransient = companion == NoSymbol || companion.isSynthetic
      def rollThroughImports(context: Context): Context = {
        if (context.isInstanceOf[ImportContext]) rollThroughImports(context.outer)
        else context
      }
      val typer = (
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
      ).asInstanceOf[ParadiseTyper]
      import typer.ParadiseTyperErrorGen._
      def expand() = expandUntyped(typer, expandee)
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
            assert(sym.rawInfo.isInstanceOf[MaybeExpandeeCompleter])
            sym.rawInfo.completeOnlyExpansions(sym)
            assert(!sym.rawInfo.isInstanceOf[MaybeExpandeeCompleter])
          }
          val derivedTrees = attachedExpansion(sym).getOrElse(List(stat))
          val (me, others) = derivedTrees.partition(_.symbol == sym)
          me ++ expandMacroAnnotations(others)
        } else {
          List(stat)
        }
      })
    }

    // NOTE: unfortunately this won't work because templateSig is private
    // but luckily we have analyzer plugins. thanks Lukas!
    // override private def templateSig(templ: Template): Type = {
    //   val result = super.templateSig(templ)
    //   expandMacroAnnotations(templ.body)
    //   result
    // }

    override def enterValueParams(vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      mmap(vparamss) { param =>
        val sym = createAssignAndEnterSymbol(param, ValueParameterFlags)
        // TODO: this should really be unified with `finishSymbol`
        // it doesn't pose any danger to us mann-wise
        // because annotations on vparamss are processed when entering DefDef
        // but this non-uniformity might bite us later
        sym setInfo monoTypeCompleter(param) label s"paramCompleter for ${sym.name}#${sym.id}"
      }
    }

    // reimplemented to integrate with weakEnsureCompanionObject
    override def ensureCompanionObject(cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = {
      val m = companionSymbolOf(cdef.symbol, context)
      def synthesizeTree = atPos(cdef.pos.focus)(creator(cdef))
      if (m != NoSymbol && currentRun.compiles(m) && !isWeak(m)) m
      else unmarkWeak(enterSyntheticSym(synthesizeTree))
    }

    /** Does the same as `ensureCompanionObject`, but also makes sure that the returned symbol destroys itself
     *  if noone ends up using it (either by calling `ensureCompanionObject` or by `finishSymbol`).
     */
    // TODO: deduplicate
    def weakEnsureCompanionObject(cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = {
      val m = companionSymbolOf(cdef.symbol, context)
      if (m != NoSymbol && currentRun.compiles(m)) m
      else { val mdef = atPos(cdef.pos.focus)(creator(cdef)); enterSym(mdef); markWeak(mdef.symbol) }
    }

    /** =================== COPY/PASTE ===================
     *  Methods copy/pasted from scalac because of either extensiblity or visibility problems.
     */
    def typeErrorHandler[T](tree: Tree, alt: T): PartialFunction[Throwable, T] = {
      case ex: TypeError =>
        // H@ need to ensure that we handle only cyclic references
        TypeSigError(tree, ex)
        alt
    }
    def contextFile = context.unit.source.file
    def validateCompanionDefs(tree: ImplDef): Unit = invokeNamer[Unit]("validateCompanionDefs", tree)
    def createPackageSymbol(pos: Position, pid: RefTree): Symbol = invokeNamer[Symbol]("createPackageSymbol", pos, pid)
    def createImportSymbol(tree: Tree): Symbol = invokeNamer[Symbol]("createImportSymbol", tree)
    def createMemberSymbol(tree: MemberDef, name: Name, mask: Long): Symbol = invokeNamer[Symbol]("createMemberSymbol", tree, name, mask)
    def invokeNamer[T](method: String, args: Any*): T = {
      // DOESN'T WORK: val reflectiveThis = this.asInstanceOf[{ def createPackageSymbol(pos: Position, pid: RefTree): Symbol }]
      // CRASHES THE COMPILER: classOf[scala.tools.nsc.typechecker.Namers$Namer].getDeclaredMethods.find(_.getName == "createPackageSymbol").get
      val m = classOf[Namer].getDeclaredMethods.find(_.getName == method).get
      m.setAccessible(true)
      m.invoke(this, args.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[T]
    }
    def _lockedCount: Int = Namers.this.lockedCount
    def _lockedCount_=(_lockedCount: Int): Unit = {
      type NscNamers = scala.tools.nsc.typechecker.Namers
      val m = classOf[NscNamers].getDeclaredMethods.find(_.getName == "scala$tools$nsc$typechecker$Namers$$_lockedCount_$eq").get
      m.setAccessible(true)
      m.invoke(Namers.this, _lockedCount.asInstanceOf[AnyRef])
    }

    /** =================== LIMBO ===================
     *  These methods have been refactored away and unified in the overridden enterSym.
     *  If someone calls into them, then something is fundamentally wrong and should fail fast.
     */
    override def assignSymbol(tree: Tree) = installationFailure
    override def assignSymbol(tree: MemberDef, name: Name, mask: Long) = installationFailure
    override def assignAndEnterSymbol(tree: MemberDef) = installationFailure
    override def assignAndEnterFinishedSymbol(tree: MemberDef) = installationFailure
    // override private def logAssignSymbol(tree: Tree, sym: Symbol) = installationFailure
    // override private def enterClassSymbol(tree: ClassDef, clazz: ClassSymbol) = installationFailure
    override def enterClassSymbol(tree: ClassDef) = installationFailure
    override def enterClassDef(tree: ClassDef) = installationFailure
    override def enterModuleSymbol(tree: ModuleDef) = installationFailure
    override def enterModuleDef(tree: ModuleDef) = installationFailure
    override def enterValDef(tree: ValDef) = installationFailure
    override def enterPackage(tree: PackageDef) = installationFailure
    override def enterTypeDef(tree: TypeDef) = installationFailure
    override def enterDefDef(tree: DefDef) = installationFailure

    /** =================== DEBUG ===================
     *  Code below doesn't implement anything crucial to how macro paradise plugin operates.
     *  It's only meant to simplify debugging by assigning labels to different sorts of completers.
     */
     override def enterCopyMethod(copyDef: DefDef) = super.enterCopyMethod(copyDef) label s"copyDefCompleter"
     override def completerOf(tree: Tree) = super.completerOf(tree) // TODO: go through the trouble of overriding kinds for both mono and poly completers
     override def enterValSymbol(tree: ValDef, sym: TermSymbol) = super.enterValSymbol(tree, sym) label s"monoTypeCompleter for val ${tree.name}"
     override def moduleClassTypeCompleter(tree: ModuleDef) = super.moduleClassTypeCompleter(tree) label s"moduleClassTypeCompleter for ${tree.symbol.rawname}#${tree.symbol.id}"
     override def accessorTypeCompleter(tree: ValDef, isSetter: Boolean) = super.accessorTypeCompleter(tree, isSetter) label s"accessorTypeCompleter for ${tree.name}, isSetter = $isSetter"
     override def selfTypeCompleter(tree: Tree) = super.selfTypeCompleter(tree) label s"selfTypeCompleter for $tree"
  }

  override def mkTypeCompleter(t: Tree)(c: Symbol => Unit) = {
    new LockingTypeCompleter with KindCompleter with FlagAgnosticCompleter {
      val tree = t
      def completeImpl(sym: Symbol) = c(sym)
    }
  }
  trait KindCompleter extends LockingTypeCompleter {
    var _kind = "TypeCompleter"
    override def kind = _kind
    def kind_=(kind: String) = _kind = kind
  }
  implicit class ParadiseCompleter[T <: TypeCompleter](tc: T) {
    def label(label: String) = {
      tc match {
        case kind: KindCompleter => kind.kind = label
        case _ => ()
      }
      tc
    }
  }
  implicit class ParadiseCompletee[T <: Symbol](sym: T) {
    def label(label: String) = {
      sym.rawInfo match {
        case tc: TypeCompleter => new ParadiseCompleter(tc).label(label)
        case _ => ()
      }
      sym
    }
  }
}
