package org.scalalang.macroparadise
package typechecker

trait ContextErrors {
  self: Analyzer =>

  import global._

  trait ParadiseTyperContextErrors extends TyperContextErrors {
    self: Typer =>

    import ErrorUtils._
    import infer.setError

    object ParadiseTyperErrorGen {
      implicit val contextTyperErrorGen: Context = infer.getContext

      def MacroAnnotationShapeError(clazz: Symbol) = {
        val sym = clazz.info.member(paradiseNme.macroTransform)
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
    }
  }
}
