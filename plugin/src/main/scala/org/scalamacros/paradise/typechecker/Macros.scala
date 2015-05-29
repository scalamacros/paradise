package org.scalamacros.paradise
package typechecker

trait Macros {
  self: Analyzer =>

  import global._
  import definitions._
  import treeInfo._
  import scala.language.reflectiveCalls
  import scala.reflect.internal.util.Statistics
  import scala.tools.nsc.typechecker.MacrosStats._
  import scala.util.control.ControlThrowable
  import scala.reflect.macros.runtime.AbortMacroException
  import scala.reflect.internal.Flags._

  override def typedMacroBody(typer: Typer, ddef: DefDef): Tree = {
    val result = invokeSuper("typedMacroBody", typer, ddef).asInstanceOf[Tree]
    val clazz = typer.context.owner.owner
    val isMacroAnnot = clazz isNonBottomSubClass AnnotationClass // NOTE: that's an approximation, see 2.11.x
    if (ddef.name == nme.macroTransform && isMacroAnnot) {
      val binding = loadMacroImplBinding(ddef.symbol)
      val message =
        "implementation restriction: macro annotation impls cannot have typetag context bounds " +
        "(consider taking apart c.macroApplication and manually calling c.typecheck on the type arguments)"
      val hasTags = binding.signature.exists(_ >= 0)
      if (hasTags) { typer.context.error(ddef.pos, message); EmptyTree }
      else result
    } else {
      result
    }
  }

  private sealed abstract class MacroExpansionResult
  private case class Success(expanded: Tree) extends MacroExpansionResult
  private case class Delay(delayed: Tree) extends MacroExpansionResult
  private case class Fallback(fallback: Tree) extends MacroExpansionResult
  private case class Other(result: Tree) extends MacroExpansionResult
  private def Skip(expanded: Tree) = Other(expanded)
  private def Cancel(expandee: Tree) = Other(expandee)
  private def Failure(expandee: Tree) = Other(expandee)

  def macroExpandUntyped(typer: Typer, expandee: Tree): Option[Tree] = {
    macroExpand1(typer, expandee) match {
      case Success(expanded) => Some(expanded)
      case _ => None
    }
  }

  override def macroExpand(typer: Typer, expandee: Tree, mode: Int = EXPRmode, pt: Type = WildcardType): Tree = {
    if (settings.Ymacronoexpand.value) return expandee // SI-6812
    val start = if (Statistics.canEnable) Statistics.startTimer(macroExpandNanos) else null
    if (Statistics.canEnable) Statistics.incCounter(macroExpandCount)
    try {
      macroExpand1(typer, expandee) match {
        case Success(expanded) =>
          try {
            def typecheck(phase: String, tree: Tree, pt: Type): Tree = {
              if (tree.isErroneous) return tree
              macroLogVerbose(s"typechecking against $phase $pt: $expanded")
              val numErrors    = reporter.ERROR.count
              def hasNewErrors = reporter.ERROR.count > numErrors
              val result = typer.context.withImplicitsEnabled(typer.typed(tree, EXPRmode, pt))
              macroLogVerbose(s"""${if (hasNewErrors) "failed to typecheck" else "successfully typechecked"} against $phase $pt:\n$result""")
              result
            }
            // approximation is necessary for whitebox macros to guide type inference
            // read more in the comments for `case Delay(delayed) => ...` below
            def approximate(tp: Type) = {
              val undetparams = tp collect { case tp if tp.typeSymbol.isTypeParameter => tp.typeSymbol }
              deriveTypeWithWildcards(undetparams)(tp)
            }
            val macroPtApprox = approximate(if (isNullaryInvocation(expandee)) expandee.tpe.finalResultType else expandee.tpe)
            // also see http://groups.google.com/group/scala-internals/browse_thread/thread/492560d941b315cc
            val expanded0 = duplicateAndKeepPositions(expanded)
            val expanded1 = typecheck("macro def return type", expanded0, macroPtApprox)
            val expanded2 = typecheck("expected type", expanded1, pt)
            expanded2
          } finally {
            popMacroContext()
          }
        case Delay(delayed) =>
          // =========== THE SITUATION ===========
          //
          // If we've been delayed (i.e. bailed out of the expansion because of undetermined type params present in the expandee),
          // then there are two possible situations we're in:
          //
          // 1) We're in POLYmode, when the typer tests the waters wrt type inference
          // (e.g. as in typedArgToPoly in doTypedApply).
          //
          // 2) We're out of POLYmode, which means that the typer is out of tricks to infer our type
          // (e.g. if we're an argument to a function call, then this means that no previous argument lists
          // can determine our type variables for us).
          //
          // Situation #1 is okay for us, since there's no pressure. In POLYmode we're just verifying that
          // there's nothing outrageously wrong with our undetermined type params (from what I understand!).
          //
          // Situation #2 requires measures to be taken. If we're in it, then noone's going to help us infer
          // the undetermined type params. Therefore we need to do something ourselves or otherwise this
          // expandee will forever remaing not expanded (see SI-5692).
          //
          // A traditional way out of this conundrum is to call `instantiate` and let the inferencer
          // try to find the way out. It works for simple cases, but sometimes, if the inferencer lacks
          // information, it will be forced to approximate.
          //
          // =========== THE PROBLEM ===========
          //
          // Consider the following example (thanks, Miles!):
          //
          //   // Iso represents an isomorphism between two datatypes:
          //   // 1) An arbitrary one (e.g. a random case class)
          //   // 2) A uniform representation for all datatypes (e.g. an HList)
          //   trait Iso[T, U] {
          //     def to(t : T) : U
          //     def from(u : U) : T
          //   }
          //   implicit def materializeIso[T, U]: Iso[T, U] = macro ???
          //
          //   case class Foo(i: Int, s: String, b: Boolean)
          //   def foo[C, L](c: C)(implicit iso: Iso[C, L]): L = iso.to(c)
          //   foo(Foo(23, "foo", true))
          //
          // In the snippet above, even though we know that there's a fundep going from T to U
          // (in a sense that a datatype's uniform representation is unambiguously determined by the datatype,
          // e.g. for Foo it will be Int :: String :: Boolean :: HNil), there's no way to convey this information
          // to the typechecker. Therefore the typechecker will infer Nothing for L, which is hardly what we want.
          //
          // =========== THE SOLUTION ===========
          //
          // To give materializers a chance to say their word before vanilla inference kicks in,
          // we infer as much as possible (e.g. in the example above even though L is hopeless, C still can be inferred to Foo)
          // and then trigger macro expansion with the undetermined type parameters still there.
          // Thanks to that the materializer can take a look at what's going on and react accordingly.
          val shouldInstantiate = typer.context.undetparams.nonEmpty && !inPolyMode(mode)
          if (shouldInstantiate) {
            forced += delayed
            typer.infer.inferExprInstance(delayed, typer.context.extractUndetparams(), pt, keepNothings = false)
            macroExpand(typer, delayed, mode, pt)
          } else delayed
        case Fallback(fallback) =>
          typer.context.withImplicitsEnabled(typer.typed(fallback, EXPRmode, pt))
        case Other(result) =>
          result
      }
    } finally {
      if (Statistics.canEnable) Statistics.stopTimer(macroExpandNanos, start)
    }
  }

  private def macroExpand1(typer: Typer, expandee: Tree): MacroExpansionResult = {
    // verbose printing might cause recursive macro expansions, so I'm shutting it down here
    withInfoLevel(nodePrinters.InfoLevel.Quiet) {
      if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
        val reason = if (expandee.symbol.isErroneous) "not found or incompatible macro implementation" else "erroneous arguments"
        macroLogVerbose(s"cancelled macro expansion because of $reason: $expandee")
        return Cancel(typer.infer.setError(expandee))
      }

      try {
        val runtime = macroRuntime(expandee.symbol)
        if (runtime != null) macroExpandWithRuntime(typer, expandee, runtime)
        else macroExpandWithoutRuntime(typer, expandee)
      } catch {
        case typer.TyperErrorGen.MacroExpansionException => Failure(expandee)
      }
    }
  }

  private def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): MacroExpansionResult = {
    val wasDelayed  = isDelayed(expandee)
    val undetparams = calculateUndetparams(expandee)
    val nowDelayed  = !typer.context.macrosEnabled || undetparams.nonEmpty

    (wasDelayed, nowDelayed) match {
      case (true, true) =>
        Delay(expandee)
      case (true, false) =>
        val expanded = macroExpandAll(typer, expandee)
        if (expanded exists (_.isErroneous)) Failure(expandee)
        else Skip(expanded)
      case (false, true) =>
        macroLogLite("macro expansion is delayed: %s".format(expandee))
        delayed += expandee -> undetparams
        expandee updateAttachment MacroRuntimeAttachment(delayed = true, typerContext = typer.context, macroContext = Some(macroArgs(typer, expandee).c))
        Delay(expandee)
      case (false, false) =>
        import typer.TyperErrorGen._
        macroLogLite("performing macro expansion %s at %s".format(expandee, expandee.pos))
        val args = macroArgs(typer, expandee)
        try {
          val numErrors    = reporter.ERROR.count
          def hasNewErrors = reporter.ERROR.count > numErrors
          val expanded = { pushMacroContext(args.c); runtime(args) }
          if (hasNewErrors) MacroGeneratedTypeError(expandee)
          expanded match {
            case expanded: Expr[_] =>
              macroLogVerbose("original:")
              macroLogLite("" + expanded.tree + "\n" + showRaw(expanded.tree))
              val freeSyms = expanded.tree.freeTerms ++ expanded.tree.freeTypes
              freeSyms foreach (sym => MacroFreeSymbolError(expandee, sym))
              Success(atPos(enclosingMacroPosition.focus)(expanded.tree updateAttachment MacroExpansionAttachment(expandee)))
            case _ =>
              MacroExpansionIsNotExprError(expandee, expanded)
          }
        } catch {
          case ex: Throwable =>
            popMacroContext()
            val realex = unwrapThrowable(ex)
            realex match {
              case ex: AbortMacroException => MacroGeneratedAbort(expandee, ex)
              case ex: ControlThrowable => throw ex
              case ex: TypeError => MacroGeneratedTypeError(expandee, ex)
              case _ => MacroGeneratedException(expandee, realex)
            }
        } finally {
          expandee.removeAttachment[MacroRuntimeAttachment]
        }
    }
  }

  private def macroExpandWithoutRuntime(typer: Typer, expandee: Tree): MacroExpansionResult = {
    import typer.TyperErrorGen._
    val fallbackSym = expandee.symbol.nextOverriddenSymbol orElse MacroImplementationNotFoundError(expandee)
    macroLogLite(s"falling back to: $fallbackSym")

    def mkFallbackTree(tree: Tree): Tree = {
      tree match {
        case Select(qual, name) => Select(qual, name) setPos tree.pos setSymbol fallbackSym
        case Apply(fn, args) => Apply(mkFallbackTree(fn), args) setPos tree.pos
        case TypeApply(fn, args) => TypeApply(mkFallbackTree(fn), args) setPos tree.pos
      }
    }
    Fallback(mkFallbackTree(expandee))
  }

  private val forced = perRunCaches.newWeakSet[Tree]
  private val delayed = perRunCaches.newWeakMap[Tree, scala.collection.mutable.Set[Int]]()
  private def isDelayed(expandee: Tree) = delayed contains expandee
  private def calculateUndetparams(expandee: Tree): scala.collection.mutable.Set[Int] =
    if (forced(expandee)) scala.collection.mutable.Set[Int]()
    else delayed.getOrElse(expandee, {
      val calculated = scala.collection.mutable.Set[Symbol]()
      expandee foreach (sub => {
        def traverse(sym: Symbol) = if (sym != null && (undetparams contains sym.id)) calculated += sym
        if (sub.symbol != null) traverse(sub.symbol)
        if (sub.tpe != null) sub.tpe foreach (sub => traverse(sub.typeSymbol))
      })
      macroLogVerbose("calculateUndetparams: %s".format(calculated))
      calculated map (_.id)
    })
  private val undetparams = perRunCaches.newSet[Int]()
  override def notifyUndetparamsAdded(newUndets: List[Symbol]): Unit = {
    undetparams ++= newUndets map (_.id)
    if (macroDebugVerbose) newUndets foreach (sym => println("undetParam added: %s".format(sym)))
  }
  override def notifyUndetparamsInferred(undetNoMore: List[Symbol], inferreds: List[Type]): Unit = {
    undetparams --= undetNoMore map (_.id)
    if (macroDebugVerbose) (undetNoMore zip inferreds) foreach { case (sym, tpe) => println("undetParam inferred: %s as %s".format(sym, tpe))}
    if (!delayed.isEmpty)
      delayed.toList foreach {
        case (expandee, undetparams) if !undetparams.isEmpty =>
          undetparams --= undetNoMore map (_.id)
          if (undetparams.isEmpty) {
            hasPendingMacroExpansions = true
            macroLogVerbose(s"macro expansion is pending: $expandee")
          }
        case _ =>
          // do nothing
      }
  }

  override def macroExpandAll(typer: Typer, expandee: Tree): Tree =
    new Transformer {
      override def transform(tree: Tree) = super.transform(tree match {
        // todo. expansion should work from the inside out
        case tree if (delayed contains tree) && calculateUndetparams(tree).isEmpty =>
          val context = tree.attachments.get[MacroRuntimeAttachment].get.typerContext
          delayed -= tree
          context.implicitsEnabled = typer.context.implicitsEnabled
          context.enrichmentEnabled = typer.context.enrichmentEnabled
          context.macrosEnabled = typer.context.macrosEnabled
          macroExpand(newTyper(context), tree, EXPRmode, WildcardType)
        case _ =>
          tree
      })
    }.transform(expandee)

  private def pushMacroContext(c: MacroContext) = invokeSuper("pushMacroContext", c).asInstanceOf[Unit]
  private def popMacroContext() = invokeSuper("popMacroContext")
  private def macroArgs(typer: Typer, expandee: Tree): MacroArgs = invokeSuper("macroArgs", typer, expandee).asInstanceOf[MacroArgs]
  private def macroRuntime(macroDef: Symbol): MacroRuntime = invokeSuper("macroRuntime", macroDef).asInstanceOf[MacroRuntime]

  private case class MacroImplBinding(signature: List[Int])
  private def loadMacroImplBinding(macroDef: Symbol): MacroImplBinding = {
    val superBinding = invokeSuper("loadMacroImplBinding", macroDef).asInstanceOf[{ val signature: List[Int] }]
    MacroImplBinding(superBinding.signature)
  }

  def invokeSuper(name: String, args: AnyRef*): AnyRef = {
    def invokeTraitPrivateMethod(clazz: Class[_], name: String, args: AnyRef*): AnyRef = {
      try {
        val traitImpl = Class.forName(clazz.getName + "$class")
        val meth = traitImpl.getDeclaredMethods.filter(m => m.getName == name || m.getName.endsWith("$" + name)).head
        meth.setAccessible(true)
        meth.invoke(null, args: _*)
      } catch unwrapHandler({ case ex => throw ex })
    }
    invokeTraitPrivateMethod(classOf[scala.tools.nsc.typechecker.Macros], name, (self +: args): _*)
  }
}
