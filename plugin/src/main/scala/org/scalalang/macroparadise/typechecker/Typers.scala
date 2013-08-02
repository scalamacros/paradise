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

    override def typedClassDef(cdef: ClassDef): Tree = {
      val cdef1 = super.typedClassDef(cdef)
      val clazz = cdef1.symbol
      if (!isPastTyper) {
        if (clazz isNonBottomSubClass AnnotationClass) {
          val macroTransform = clazz.info.member(paradiseNme.macroTransform)
          if (macroTransform != NoSymbol) {
            def flavorOk = macroTransform.isMacro
            def paramssOk = mmap(macroTransform.paramss)(p => (p.name, p.info)) == List(List((paradiseNme.annottees, scalaRepeatedType(AnyTpe))))
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
