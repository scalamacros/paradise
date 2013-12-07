package org.scalalang.macroparadise
package typechecker

trait Errors {
  self: AnalyzerPlugins =>

  import global._
  import analyzer._
  import ErrorUtils._

  class ErrorGen(val typer: Typer) {
    implicit val contextTyperErrorGen: Context = typer.infer.getContext

    def MacroAnnotationShapeError(clazz: Symbol) = {
      val sym = clazz.info.member(nme.macroTransform)
      var actualSignature = sym.toString
      if (sym.isOverloaded) actualSignature += "(...) = ..."
      else if (sym.isMethod) {
        if (sym.typeParams.nonEmpty) {
          def showTparam(tparam: Symbol) =
            tparam.typeSignature match {
              case tpe @ TypeBounds(_, _) => s"${tparam.name}$tpe"
              case _ => tparam.name
            }
          def showTparams(tparams: List[Symbol]) = "[" + (tparams map showTparam mkString ", ") + "]"
          actualSignature += showTparams(sym.typeParams)
        }
        if (sym.paramss.nonEmpty) {
          def showParam(param: Symbol) = s"${param.name}: ${param.typeSignature}"
          def showParams(params: List[Symbol]) = {
            val s_mods = if (params.nonEmpty && params(0).hasFlag(scala.reflect.internal.Flags.IMPLICIT)) "implicit " else ""
            val s_params = params map showParam mkString ", "
            "(" + s_mods + s_params + ")"
          }
          def showParamss(paramss: List[List[Symbol]]) = paramss map showParams mkString ""
          actualSignature += showParamss(sym.paramss)
        }
        if (sym.isTermMacro) actualSignature = actualSignature.replace("macro method", "def") + " = macro ..."
        else actualSignature = actualSignature.replace("method", "def") + " = ..."
      }
      issueSymbolTypeError(clazz, s"""
        |macro annotation has wrong shape:
        |  required: def macroTransform(annottees: Any*) = macro ...
        |  found   : $actualSignature
      """.trim.stripMargin)
    }

    def MacroAnnotationMustBeStaticError(clazz: Symbol) =
      issueSymbolTypeError(clazz, s"macro annotation must extend scala.annotation.StaticAnnotation")

    def MacroAnnotationCannotBeInheritedError(clazz: Symbol) =
      issueSymbolTypeError(clazz, s"macro annotation cannot be @Inherited")

    def MacroAnnotationCannotBeMemberError(clazz: Symbol) =
      issueSymbolTypeError(clazz, s"macro annotation cannot be a member of another class")

    def MacroAnnotationNotExpandedMessage = {
      "macro annotation could not be expanded " +
      "(the most common reason for that is that you need to enable the macro paradise plugin; " +
      "another possibility is that you try to use macro annotation in the same compilation run that defines it)"
    }

    def MacroAnnotationOnlyDefinitionError(ann: Tree) =
      issueNormalTypeError(ann, "macro annotations can only be put on definitions")

    def MacroAnnotationTopLevelClassWithCompanionBadExpansion(ann: Tree) =
      issueNormalTypeError(ann, "top-level class with companion can only expand into a block consisting in eponymous companions")

    def MacroAnnotationTopLevelClassWithoutCompanionBadExpansion(ann: Tree) =
      issueNormalTypeError(ann, "top-level class without companion can only expand either into an eponymous class or into a block consisting in eponymous companions")

    def MacroAnnotationTopLevelModuleBadExpansion(ann: Tree) =
      issueNormalTypeError(ann, "top-level object can only expand into an eponymous object")

    def MultipleParametersImplicitClassError(tree: Tree) =
      issueNormalTypeError(tree, "implicit classes must accept exactly one primary constructor parameter")
  }
}
