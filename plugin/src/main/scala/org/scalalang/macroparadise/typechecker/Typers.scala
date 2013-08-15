package org.scalalang.macroparadise
package typechecker

trait Typers {
  self: Analyzer =>

  import global._
  import definitions._
  import paradiseDefinitions._
  import scala.reflect.internal.Flags._
  import scala.reflect.internal.Mode

  trait ParadiseTyper extends Typer with ParadiseTyperContextErrors {
    import ParadiseTyperErrorGen._

    private def isPastTyper = phase.id > currentRun.typerPhase.id
    override def namer = super.namer.asInstanceOf[ParadiseNamer]

    override def reallyExists(sym: Symbol) = {
       if (isStale(sym)) sym.setInfo(NoType)
       (!isWeak(sym) || sym.info != NoType) && sym.exists
    }

    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
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

    override def typedBlock(block: Block, mode: Mode, pt: Type): Block = {
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
  }
}
