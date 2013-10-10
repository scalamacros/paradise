package org.scalalang.macroparadise
package typechecker

trait Infer {
  self: Analyzer =>

  import global._
  import definitions._
  import typer.printInference
  import typeDebug._

  class ParadiseInferencer(context: Context) extends Inferencer(context) {
    import InferErrorGen._

    def exprTypeArgs(tparams: List[Symbol], restpe: Type, pt: Type, useWeaklyCompatible: Boolean): (List[Type], List[TypeVar]) = {
      val tvars = tparams map freshVar
      val instResTp = restpe.instantiateTypeParams(tparams, tvars)
      if ( if (useWeaklyCompatible) isWeaklyCompatible(instResTp, pt) else isCompatible(instResTp, pt) ) {
        try {
          // If the restpe is an implicit method, and the expected type is fully defined
          // optimize type variables wrt to the implicit formals only; ignore the result type.
          // See test pos/jesper.scala
          val varianceType = restpe match {
            case mt: MethodType if mt.isImplicit && isFullyDefined(pt) =>
              MethodType(mt.params, AnyClass.tpe)
            case _ =>
              restpe
          }
          //println("try to solve "+tvars+" "+tparams)
          (solvedTypes(tvars, tparams, tparams map varianceInType(varianceType),
                      false, lubDepth(List(restpe, pt))), tvars)
        } catch {
          case ex: NoInstance => (null, null)
        }
      } else (null, null)
    }

    def isCompatible(tp: Type, pt: Type): Boolean = {
      def isCompatibleByName(tp: Type, pt: Type): Boolean = pt match {
        case TypeRef(_, ByNameParamClass, List(res)) if !isByNameParamType(tp) => isCompatible(tp, res)
        case _ => false
      }
      val tp1 = normalize(tp)
      (tp1 weak_<:< pt) || isCoercible(tp1, pt) || isCompatibleByName(tp, pt)
    }

    def isFullyDefined(tp: Type): Boolean = tp match {
      case WildcardType | BoundedWildcardType(_) | NoType =>
        false
      case NoPrefix | ThisType(_) | ConstantType(_) =>
        true
      case TypeRef(pre, sym, args) =>
        isFullyDefined(pre) && (args forall isFullyDefined)
      case SingleType(pre, sym) =>
        isFullyDefined(pre)
      case RefinedType(ts, decls) =>
        ts forall isFullyDefined
      case TypeVar(origin, constr) if (constr.inst == NoType) =>
        false
      case _ =>
        try {
          instantiate(tp); true
        } catch {
          case ex: NoInstance => false
        }
    }

    def substExpr(tree: Tree, undetparams: List[Symbol], targs: List[Type], pt: Type): Unit = {
      if (targs eq null) {
        if (!tree.tpe.isErroneous && !pt.isErroneous)
          PolymorphicExpressionInstantiationError(tree, undetparams, pt)
      } else {
        new TreeTypeSubstituter(undetparams, targs).traverse(tree)
        notifyUndetparamsInferred(undetparams, targs)
      }
    }

    override def inferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type = WildcardType, treeTp0: Type = null, keepNothings: Boolean = true, useWeaklyCompatible: Boolean = false): List[Symbol] = {
      val treeTp = if(treeTp0 eq null) tree.tpe else treeTp0 // can't refer to tree in default for treeTp0
      val (targs, tvars) = exprTypeArgs(tparams, treeTp, pt, useWeaklyCompatible)
      def dropByName(tp: Type) = tp match { case TypeRef(_, _, targ :: Nil) if definitions.isByNameParamType(tp) => targ; case _ => tp }
      def targsStrict = if (targs eq null) null else targs mapConserve dropByName
      printInference(
        ptBlock("inferExprInstance",
          "tree"    -> tree,
          "tree.tpe"-> tree.tpe,
          "tparams" -> tparams,
          "pt"      -> pt,
          "targs"   -> targs,
          "tvars"   -> tvars
        )
      )

      if (keepNothings || (targs eq null)) { //@M: adjustTypeArgs fails if targs==null, neg/t0226
        substExpr(tree, tparams, targsStrict, pt)
        List()
      } else {
        val AdjustedTypeArgs.Undets(okParams, okArgs, leftUndet) = adjustTypeArgs(tparams, tvars, targsStrict)
        printInference(
          ptBlock("inferExprInstance/AdjustedTypeArgs",
            "okParams" -> okParams,
            "okArgs" -> okArgs,
            "leftUndet" -> leftUndet
          )
        )
        substExpr(tree, okParams, okArgs, pt)
        leftUndet
      }
    }
  }
}