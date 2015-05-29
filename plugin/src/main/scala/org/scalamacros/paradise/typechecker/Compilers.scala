package org.scalamacros.paradise
package typechecker

trait Compilers {
  self: AnalyzerPlugins =>

  import global._
  import analyzer._
  import definitions._
  import paradiseDefinitions._
  import scala.reflect.internal.Flags._

  def mkCompiler(typer: Typer) = new Compiler(typer)
  class Compiler(typer: Typer) {
    val errorGen = new ErrorGen(typer)
    import errorGen._

    def typedMacroAnnotation(cdef: ClassDef) = {
      val clazz = cdef.symbol
      if (!isPastTyper) {
        if (clazz != null && (clazz isNonBottomSubClass AnnotationClass)) {
          val macroTransform = clazz.info.member(nme.macroTransform)
          if (macroTransform != NoSymbol) {
            clazz.setFlag(MACRO)
            clazz.addAnnotation(AnnotationInfo(CompileTimeOnlyAttr.tpe, List(Literal(Constant(MacroAnnotationNotExpandedMessage)) setType StringClass.tpe), Nil))
            def flavorOk = macroTransform.isMacro
            def paramssOk = mmap(macroTransform.paramss)(p => (p.name, p.info)) == List(List((nme.annottees, scalaRepeatedType(AnyTpe))))
            def tparamsOk = macroTransform.typeParams.isEmpty
            def everythingOk = flavorOk && paramssOk && tparamsOk
            if (!everythingOk) MacroAnnotationShapeError(clazz)
            if (!(clazz isNonBottomSubClass StaticAnnotationClass)) MacroAnnotationMustBeStaticError(clazz)
            // TODO: revisit the decision about @Inherited
            if (clazz.getAnnotation(InheritedAttr).nonEmpty) MacroAnnotationCannotBeInheritedError(clazz)
            if (!clazz.isStatic) MacroAnnotationCannotBeMemberError(clazz)
          }
        }
      }
      cdef
    }
  }
}