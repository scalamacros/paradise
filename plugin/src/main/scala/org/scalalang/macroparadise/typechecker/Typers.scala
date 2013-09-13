package org.scalalang.macroparadise
package typechecker

trait Typers {
  self: Analyzer =>

  import global._
  import definitions._
  import paradiseDefinitions._
  import scala.reflect.internal.Flags._

  trait ParadiseTyper extends Typer with ParadiseTyperContextErrors {
    import ParadiseTyperErrorGen._

    private def isPastTyper = phase.id > currentRun.typerPhase.id
    override def namer = super.namer.asInstanceOf[ParadiseNamer]

    override def reallyExists(sym: Symbol) = {
       if (isStale(sym)) sym.setInfo(NoType)
       (!isWeak(sym) || sym.info != NoType) && sym.exists
    }

    override def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      def typedAnnotated(atd: Annotated): Tree = {
        val result = super.typed1(tree, mode, pt)
        val anns = if (result.tpe != null) result.tpe.annotations else Nil
        val ann = anns.headOption getOrElse UnmappableAnnotation
        if (ann.atp.typeSymbol.isMacro) MacroAnnotationOnlyDefinitionError(atd.annot)
        result
      }

      def typedPackageDef(pdef: PackageDef) = {
        val PackageDef(pid, stats) = pdef
        val stats1 = namer.expandMacroAnnotations(pdef.stats)
        super.typed1(treeCopy.PackageDef(pdef, pid, stats1), mode, pt)
      }

      val sym: Symbol = tree.symbol
      if ((sym ne null) && (sym ne NoSymbol)) sym.initialize

      tree match {
        case tree: Annotated  => typedAnnotated(tree)
        case tree: PackageDef => typedPackageDef(tree)
        case _                => super.typed1(tree, mode, pt)
      }
    }

    override def typedTemplate(templ: Template, parents1: List[Tree]): Template = {
      val Template(parents, self, body) = templ
      val body1 = namer.expandMacroAnnotations(body)
      super.typedTemplate(treeCopy.Template(templ, parents, self, body1), parents1)
    }

    override def typedBlock(block: Block, mode: Int, pt: Type): Block = {
      val Block(stats, expr) = block
      namer.enterSyms(stats)
      val stats1 = namer.expandMacroAnnotations(stats)
      super.typedBlock(treeCopy.Block(block, stats1, expr), mode, pt)
    }

    override def typedClassDef(cdef: ClassDef): Tree = {
      val cdef1 = super.typedClassDef(cdef)
      val clazz = cdef1.symbol
      if (!isPastTyper) {
        if (clazz != null && (clazz isNonBottomSubClass AnnotationClass)) {
          val macroTransform = clazz.info.member(nme.macroTransform)
          if (macroTransform != NoSymbol) {
            def flavorOk = macroTransform.isMacro
            def paramssOk = mmap(macroTransform.paramss)(p => (p.name, p.info)) == List(List((nme.annottees, scalaRepeatedType(AnyTpe))))
            def tparamsOk = macroTransform.typeParams.isEmpty
            def everythingOk = flavorOk && paramssOk && tparamsOk
            if (!everythingOk) MacroAnnotationShapeError(clazz)
            if (!(clazz isNonBottomSubClass StaticAnnotationClass)) MacroAnnotationMustBeStaticError(clazz)
            // TODO: revisit the decision about @Inherited
            if (clazz.getAnnotation(InheritedAttr).nonEmpty) MacroAnnotationCannotBeInheritedError(clazz)
            if (!clazz.isStatic) MacroAnnotationCannotBeMemberError(clazz)
            clazz.setFlag(MACRO)
            // TODO: can't do this until it's scala.annotation.compileTimeOnly
            // otherwise we'll force our users to have scala-reflect.jar on classpath
            // clazz.addAnnotation(AnnotationInfo(CompileTimeOnlyAttr.tpe, List(Literal(Constant(MacroAnnotationNotExpandedMessage)) setType StringClass.tpe), Nil))
          }
        }
      }
      cdef1
    }

    override def doTypedUnapply(tree: Tree, fun0: Tree, fun: Tree, args: List[Tree], mode: Int, pt: Type): Tree = {
      val unapply = unapplyMember(fun.tpe)
      if (QuasiquoteMacros.contains(unapply)) {
        val expandee = treeCopy.Apply(tree, gen.mkAttributedSelect(fun, unapply), args)
        val expanded = duplicateAndKeepPositions(atPos(tree.pos)(expandUntyped(this, expandee).getOrElse(infer.setError(tree))))
        typed(expanded, mode, pt)
      } else {
        super.doTypedUnapply(tree, fun0, fun, args, mode, pt)
      }
    }

    // suppress existential warnings for our dependently-typed quasiquote extractors
    // see QuasiquoteCompat.scala for more information for details
    override def checkExistentialsFeature(pos: Position, tpe: Type, prefix: String): Unit = {
      def shouldSuppress(quant: Symbol) = {
        // some debug output
        // extp pretty = List[_50.u.Tree] forSome { val _50: scala.reflect.api.QuasiquoteCompat.AppliedExtractor{val u: reflect.runtime.universe.type} }
        // extp raw = ExistentialType(List(newTypeName("_50.type")), TypeRef(ThisType(scala.collection.immutable), scala.collection.immutable.List, List(TypeRef(SingleType(TypeRef(NoPrefix, newTypeName("_50.type"), List()), newTermName("u")), newTypeName("Tree"), List()))))
        // quant.tpe pretty = _50.type
        // quant.tpe raw = TypeRef(NoPrefix, newTypeName("_50.type"), List())
        // quant.info pretty = <: scala.reflect.api.QuasiquoteCompat.AppliedExtractor{val u: reflect.runtime.universe.type} with Singleton
        // quant.info raw = TypeBounds(TypeRef(ThisType(scala), scala.Nothing, List()), RefinedType(List(RefinedType(List(TypeRef(ThisType(scala.reflect.api.QuasiquoteCompat), scala.reflect.api.QuasiquoteCompat.AppliedExtractor, List())), Scope(newTermName("u"))), TypeRef(ThisType(scala), scala.Singleton, List())), Scope()))
        quant.info.exists(_.typeSymbol.sourceModule.rawname == newTermName("QuasiquoteCompat"))
      }
      tpe match {
        case extp @ ExistentialType(quants, underlying) if !extp.isRepresentableWithWildcards && quants.exists(shouldSuppress) => ()
        case _ => super.checkExistentialsFeature(pos, tpe, prefix)
      }
    }
  }
}
