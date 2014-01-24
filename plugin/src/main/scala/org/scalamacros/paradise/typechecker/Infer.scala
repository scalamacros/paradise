package org.scalamacros.paradise
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

    override def isApplicableSafe(undetparams: List[Symbol], ftpe: Type,
                         argtpes0: List[Type], pt: Type): Boolean = {
      val silentContext = context.makeSilent(false)
      val typer0 = newTyper(silentContext)
      val res1 = typer0.infer.asInstanceOf[ParadiseInferencer].isApplicable(undetparams, ftpe, argtpes0, pt)
      if (pt != WildcardType && silentContext.hasErrors) {
        silentContext.flushBuffer()
        val res2 = typer0.infer.asInstanceOf[ParadiseInferencer].isApplicable(undetparams, ftpe, argtpes0, WildcardType)
        if (silentContext.hasErrors) false else res2
      } else res1
    }

    def isApplicable(undetparams: List[Symbol], ftpe: Type,
                     argtpes0: List[Type], pt: Type): Boolean =
      ftpe match {
        case OverloadedType(pre, alts) =>
          alts exists (alt => isApplicable(undetparams, pre.memberType(alt), argtpes0, pt))
        case ExistentialType(tparams, qtpe) =>
          isApplicable(undetparams, qtpe, argtpes0, pt)
        case mt @ MethodType(params, _) =>
          val formals = formalTypes(mt.paramTypes, argtpes0.length, removeByName = false)

          def tryTupleApply: Boolean = {
            // if 1 formal, 1 argtpe (a tuple), otherwise unmodified argtpes0
            val tupleArgTpes = actualTypes(argtpes0 map {
                // no assignment is treated as named argument here
              case NamedType(name, tp) => UnitClass.tpe
              case tp => tp
              }, formals.length)

            !sameLength(argtpes0, tupleArgTpes) &&
            !isUnitForVarArgs(argtpes0, params) &&
            isApplicable(undetparams, ftpe, tupleArgTpes, pt)
          }
          def typesCompatible(argtpes: List[Type]) = {
            val restpe = ftpe.resultType(argtpes)
            if (undetparams.isEmpty) {
              isCompatibleArgs(argtpes, formals) && isWeaklyCompatible(restpe, pt)
            } else {
              try {
                val AdjustedTypeArgs.Undets(okparams, okargs, leftUndet) = methTypeArgs(undetparams, formals, restpe, argtpes, pt)
                // #2665: must use weak conformance, not regular one (follow the monomorphic case above)
                (exprTypeArgs(leftUndet, restpe.instantiateTypeParams(okparams, okargs), pt, useWeaklyCompatible = true)._1 ne null) &&
                isWithinBounds(NoPrefix, NoSymbol, okparams, okargs)
              } catch {
                case ex: NoInstance => false
              }
            }
          }

          // very similar logic to doTypedApply in typechecker
          val lencmp = compareLengths(argtpes0, formals)
          if (lencmp > 0) tryTupleApply
          else if (lencmp == 0) {
            if (!argtpes0.exists(_.isInstanceOf[NamedType])) {
              // fast track if no named arguments are used
              typesCompatible(argtpes0)
            }
            else {
              // named arguments are used
              val (argtpes1, argPos, namesOK) = checkNames(argtpes0, params)
              // when using named application, the vararg param has to be specified exactly once
              ( namesOK && (isIdentity(argPos) || sameLength(formals, params)) &&
              // nb. arguments and names are OK, check if types are compatible
                typesCompatible(reorderArgs(argtpes1, argPos))
              )
            }
          }
          else {
            // not enough arguments, check if applicable using defaults
            val missing = missingParams[Type](argtpes0, params, {
              case NamedType(name, _) => Some(name)
              case _ => None
            })._1
            if (missing forall (_.hasDefault)) {
              // add defaults as named arguments
              val argtpes1 = argtpes0 ::: (missing map (p => NamedType(p.name, p.tpe)))
              isApplicable(undetparams, ftpe, argtpes1, pt)
            }
            else tryTupleApply
          }

        case NullaryMethodType(restpe) => // strip nullary method type, which used to be done by the polytype case below
          isApplicable(undetparams, restpe, argtpes0, pt)
        case PolyType(tparams, restpe) =>
          createFromClonedSymbols(tparams, restpe)((tps1, restpe1) => isApplicable(tps1 ::: undetparams, restpe1, argtpes0, pt))
        case ErrorType =>
          true
        case _ =>
          false
      }

    def checkNames(argtpes: List[Type], params: List[Symbol]) = {
      val argPos = Array.fill(argtpes.length)(-1)
      var positionalAllowed, namesOK = true
      var index = 0
      val argtpes1 = argtpes map {
        case NamedType(name, tp) => // a named argument
          var res = tp
          val pos = params.indexWhere(p => paramMatchesName(p, name) && !p.isSynthetic)

          if (pos == -1) {
            if (positionalAllowed) { // treat assignment as positional argument
              argPos(index) = index
              res = UnitClass.tpe
            } else                   // unknown parameter name
              namesOK = false
          } else if (argPos.contains(pos)) { // parameter specified twice
            namesOK = false
          } else {
            if (index != pos)
              positionalAllowed = false
            argPos(index) = pos
          }
          index += 1
          res
        case tp => // a positional argument
          argPos(index) = index
          if (!positionalAllowed)
            namesOK = false // positional after named
          index += 1
          tp
      }
      (argtpes1, argPos, namesOK)
    }

    def paramMatchesName(param: Symbol, name: Name) =
      param.name == name || param.deprecatedParamName.exists(_ == name)
  }
}