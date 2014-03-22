package scala.quasiquotes

import scala.reflect.api._
import scala.reflect.internal.SymbolTable

object QuasiquoteCompat { def apply[U <: Universe with Singleton](u0: U): QuasiquoteCompat { val u: u0.type } = new { val u: u0.type = u0 } with QuasiquoteCompat }
trait QuasiquoteCompat {
  val u: Universe
  val _u: SymbolTable = u.asInstanceOf[SymbolTable]
  import u._, definitions._, Flag._

  // ==================== ADDITIONAL APIS THAT WE NEED IN UNIVERSE ====================

  trait UniverseApi {
  }

  val _universe: SymbolTableCompat = new { val global: _u.type = _u } with SymbolTableCompat

  object universe extends UniverseApi {
  }

  // ==================== REIFICATIONSUPPORT COPY/PASTED FROM MASTER AND ADAPTED TO BE COMPATIBLE WITH SCALA.REFLECT.API.UNIVERSE ====================

  trait ReificationSupportApi {
    /** Selects type symbol with given simple name `name` from the defined members of `owner`.
     */
    def selectType(owner: Symbol, name: String): TypeSymbol

    /** Selects term symbol with given name and type from the defined members of prefix type
     */
    def selectTerm(owner: Symbol, name: String): TermSymbol

    /** Selects overloaded method symbol with given name and index
     */
    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol

    /** A fresh symbol with given name `name`, position `pos` and flags `flags` that has
     *  the current symbol as its owner.
     */
    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: FlagSet, isClass: Boolean): Symbol

    def newScopeWith(elems: Symbol*): Scope

    /** Create a fresh free term symbol.
     *  @param   name   the name of the free variable
     *  @param   value  the value of the free variable at runtime
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol

    /** Create a fresh free type symbol.
     *  @param   name   the name of the free variable
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeType(name: String, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol

    /** Set symbol's type signature to given type.
     *  @return the symbol itself
     */
    def setInfo[S <: Symbol](sym: S, tpe: Type): S

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations[S <: Symbol](sym: S, annots: List[Annotation]): S

    def mkThis(sym: Symbol): Tree

    def mkSelect(qualifier: Tree, sym: Symbol): Select

    def mkIdent(sym: Symbol): Ident

    def mkTypeTree(tp: Type): TypeTree

    def ThisType(sym: Symbol): Type

    def SingleType(pre: Type, sym: Symbol): Type

    def SuperType(thistpe: Type, supertpe: Type): Type

    def ConstantType(value: Constant): ConstantType

    def TypeRef(pre: Type, sym: Symbol, args: List[Type]): Type

    def RefinedType(parents: List[Type], decls: Scope, typeSymbol: Symbol): RefinedType

    def ClassInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType

    def MethodType(params: List[Symbol], resultType: Type): MethodType

    def NullaryMethodType(resultType: Type): NullaryMethodType

    def PolyType(typeParams: List[Symbol], resultType: Type): PolyType

    def ExistentialType(quantified: List[Symbol], underlying: Type): ExistentialType

    def AnnotatedType(annotations: List[Annotation], underlying: Type, selfsym: Symbol): AnnotatedType

    def TypeBounds(lo: Type, hi: Type): TypeBounds

    def BoundedWildcardType(bounds: TypeBounds): BoundedWildcardType

    def thisPrefix(sym: Symbol): Type

    def setType[T <: Tree](tree: T, tpe: Type): T

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T

    def toStats(tree: Tree): List[Tree]

    def mkAnnotation(tree: Tree): Tree

    def mkAnnotation(trees: List[Tree]): List[Tree]

    def mkRefineStat(stat: Tree): Tree

    def mkRefineStat(stats: List[Tree]): List[Tree]

    def mkPackageStat(stat: Tree): Tree

    def mkPackageStat(stats: List[Tree]): List[Tree]

    def mkEarlyDef(defn: Tree): Tree

    def mkEarlyDef(defns: List[Tree]): List[Tree]

    def mkRefTree(qual: Tree, sym: Symbol): Tree

    def freshTermName(prefix: String): TermName

    def freshTypeName(prefix: String): TypeName

    val ImplicitParams: ImplicitParamsExtractor

    trait ImplicitParamsExtractor {
      def apply(paramss: List[List[Tree]], implparams: List[Tree]): List[List[Tree]]
      def unapply(vparamss: List[List[ValDef]]): Some[(List[List[ValDef]], List[ValDef])]
    }

    val ScalaDot: ScalaDotExtractor

    trait ScalaDotExtractor {
      def apply(name: Name): Tree
      def unapply(tree: Tree): Option[Name]
    }

    val FlagsRepr: FlagsReprExtractor

    trait FlagsReprExtractor {
      def apply(value: Long): FlagSet
      def unapply(flags: Long): Some[Long]
    }

    val SyntacticTypeApplied: SyntacticTypeAppliedExtractor
    val SyntacticAppliedType: SyntacticTypeAppliedExtractor

    trait SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree
      def unapply(tree: Tree): Option[(Tree, List[Tree])]
    }

    val SyntacticApplied: SyntacticAppliedExtractor

    trait SyntacticAppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree
      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])]
    }

    val SyntacticClassDef: SyntacticClassDefExtractor

    trait SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                constrMods: Modifiers, vparamss: List[List[Tree]],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticTraitDef: SyntacticTraitDefExtractor

    trait SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticObjectDef: SyntacticObjectDefExtractor

    trait SyntacticObjectDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ModuleDef
      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor

    trait SyntacticPackageObjectDefExtractor {
      def apply(name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): PackageDef
      def unapply(tree: Tree): Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticTuple: SyntacticTupleExtractor
    val SyntacticTupleType: SyntacticTupleExtractor

    trait SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree
      def unapply(tree: Tree): Option[List[Tree]]
    }

    val SyntacticBlock: SyntacticBlockExtractor

    trait SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree
      def unapply(tree: Tree): Option[List[Tree]]
    }

    val SyntacticNew: SyntacticNewExtractor

    trait SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree
      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticFunctionType: SyntacticFunctionTypeExtractor

    trait SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree
      def unapply(tree: Tree): Option[(List[Tree], Tree)]
    }

    val SyntacticFunction: SyntacticFunctionExtractor

    trait SyntacticFunctionExtractor {
      def apply(params: List[Tree], body: Tree): Function

      def unapply(tree: Function): Option[(List[ValDef], Tree)]
    }

    val SyntacticDefDef: SyntacticDefDefExtractor

    trait SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree],
                vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)]
    }

    val SyntacticValDef: SyntacticValDefExtractor
    val SyntacticVarDef: SyntacticValDefExtractor

    trait SyntacticValDefExtractor {
      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef
      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)]
    }

    val SyntacticPatDef: SyntacticPatDefExtractor

    trait SyntacticPatDefExtractor {
      def apply(mods: Modifiers, pat: Tree, tpt: Tree, rhs: Tree): List[ValDef]
    }

    val SyntacticAssign: SyntacticAssignExtractor

    trait SyntacticAssignExtractor {
      def apply(lhs: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticValFrom: SyntacticValFromExtractor

    trait SyntacticValFromExtractor {
      def apply(pat: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticValEq: SyntacticValEqExtractor

    trait SyntacticValEqExtractor {
      def apply(pat: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticFilter: SyntacticFilterExtractor

    trait SyntacticFilterExtractor {
      def apply(test: Tree): Tree
      def unapply(tree: Tree): Option[(Tree)]
    }

    val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor

    trait SyntacticEmptyTypeTreeExtractor {
      def apply(): TypeTree
      def unapply(tt: TypeTree): Boolean
    }

    val SyntacticFor: SyntacticForExtractor
    val SyntacticForYield: SyntacticForExtractor

    trait SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree
      def unapply(tree: Tree): Option[(List[Tree], Tree)]
    }

    // def UnliftListElementwise[T](unliftable: Unliftable[T]): UnliftListElementwise[T]
    // trait UnliftListElementwise[T] {
    //   def unapply(lst: List[Tree]): Option[List[T]]
    // }

    // def UnliftListOfListsElementwise[T](unliftable: Unliftable[T]): UnliftListOfListsElementwise[T]
    // trait UnliftListOfListsElementwise[T] {
    //   def unapply(lst: List[List[Tree]]): Option[List[List[T]]]
    // }

    val SyntacticPartialFunction: SyntacticPartialFunctionExtractor
    trait SyntacticPartialFunctionExtractor {
      def apply(cases: List[Tree]): Match
      def unapply(tree: Match): Option[List[CaseDef]]
    }

    val SyntacticMatch: SyntacticMatchExtractor
    trait SyntacticMatchExtractor {
      def apply(scrutinee: Tree, cases: List[Tree]): Match
      def unapply(tree: Match): Option[(Tree, List[CaseDef])]
    }

    val SyntacticTry: SyntacticTryExtractor
    trait SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree): Try
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)]
    }

    val SyntacticIdent: SyntacticIdentExtractor
    trait SyntacticIdentExtractor {
      def apply(name: Name, isBackquoted: Boolean = false): Ident
      def unapply(tree: Ident): Option[(Name, Boolean)]
    }

    val SyntacticImport: SyntacticImportExtractor
    trait SyntacticImportExtractor {
      def apply(expr: Tree, selectors: List[Tree]): Import
      def unapply(imp: Import): Some[(Tree, List[Tree])]
    }

    val SyntacticSelectType: SyntacticSelectTypeExtractor
    trait SyntacticSelectTypeExtractor {
      def apply(qual: Tree, name: TypeName): Select
      def unapply(tree: Tree): Option[(Tree, TypeName)]
    }

    val SyntacticSelectTerm: SyntacticSelectTermExtractor
    trait SyntacticSelectTermExtractor {
      def apply(qual: Tree, name: TermName): Select
      def unapply(tree: Tree): Option[(Tree, TermName)]
    }
  }

  // TODO: making this and similar fields private will crash at runtime with IllegalAccessError
  val _build = new { val global: _u.type = _u } with ReificationSupport

  val reificationSupport: build.type = build

  object build extends ReificationSupportApi {
    def AnnotatedType(annotations: List[Annotation],underlying: Type,selfSym: Symbol): AnnotatedType = _build.AnnotatedType(annotations.asInstanceOf[List[_u.Annotation]], underlying.asInstanceOf[_u.Type], selfSym.asInstanceOf[_u.Symbol]).asInstanceOf[AnnotatedType]
    def BoundedWildcardType(bounds: TypeBounds): BoundedWildcardType = _build.BoundedWildcardType(bounds.asInstanceOf[_u.TypeBounds]).asInstanceOf[BoundedWildcardType]
    def ClassInfoType(parents: List[Type],decls: Scope,typeSymbol: Symbol): ClassInfoType = _build.ClassInfoType(parents.asInstanceOf[List[_u.Type]], decls.asInstanceOf[_u.Scope], typeSymbol.asInstanceOf[_u.Symbol]).asInstanceOf[ClassInfoType]
    def ConstantType(value: Constant): ConstantType = _build.ConstantType(value.asInstanceOf[_u.Constant]).asInstanceOf[ConstantType]
    def ExistentialType(quantified: List[Symbol],underlying: Type): ExistentialType = _build.ExistentialType(quantified.asInstanceOf[List[_u.Symbol]], underlying.asInstanceOf[_u.Type]).asInstanceOf[ExistentialType]
    val FlagsRepr: FlagsReprExtractor = new FlagsReprExtractor {
      def apply(value: Long): FlagSet = _build.FlagsRepr.apply(value).asInstanceOf[FlagSet]
      def unapply(flags: Long): Some[Long] = _build.FlagsRepr.unapply(flags)
    }
    val ImplicitParams: ImplicitParamsExtractor = new ImplicitParamsExtractor {
      def apply(paramss: List[List[Tree]], implparams: List[Tree]): List[List[Tree]] = _build.ImplicitParams.apply(paramss.asInstanceOf[List[List[_u.Tree]]], implparams.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[List[Tree]]]
      def unapply(vparamss: List[List[ValDef]]): Some[(List[List[ValDef]], List[ValDef])] = _build.ImplicitParams.unapply(vparamss.asInstanceOf[List[List[_u.ValDef]]]).asInstanceOf[Some[(List[List[ValDef]], List[ValDef])]]
    }
    def MethodType(params: List[Symbol],resultType: Type): MethodType = _build.MethodType(params.asInstanceOf[List[_u.Symbol]], resultType.asInstanceOf[_u.Type]).asInstanceOf[MethodType]
    def NullaryMethodType(resultType: Type): NullaryMethodType = _build.NullaryMethodType(resultType.asInstanceOf[_u.Type]).asInstanceOf[NullaryMethodType]
    def PolyType(typeParams: List[Symbol],resultType: Type): PolyType = _build.PolyType(typeParams.asInstanceOf[List[_u.Symbol]], resultType.asInstanceOf[_u.Type]).asInstanceOf[PolyType]
    def RefinedType(parents: List[Type],decls: Scope,typeSymbol: Symbol): RefinedType = _build.RefinedType(parents.asInstanceOf[List[_u.Type]], decls.asInstanceOf[_u.Scope], typeSymbol.asInstanceOf[_u.Symbol]).asInstanceOf[RefinedType]
    val ScalaDot: ScalaDotExtractor = new ScalaDotExtractor {
      def apply(name: Name): Tree = _build.ScalaDot.apply(name.asInstanceOf[_u.Name]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[Name] = _build.ScalaDot.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[Name]]
    }
    def SingleType(pre: Type,sym: Symbol): Type = _build.SingleType(pre.asInstanceOf[_u.Type], sym.asInstanceOf[_u.Symbol]).asInstanceOf[SingleType]
    def SuperType(thistpe: Type,supertpe: Type): Type = _build.SuperType(thistpe.asInstanceOf[_u.Type], supertpe.asInstanceOf[_u.Type]).asInstanceOf[SuperType]
    val SyntacticApplied: SyntacticAppliedExtractor = new SyntacticAppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree = _build.SyntacticApplied.apply(tree.asInstanceOf[_u.Tree], argss.asInstanceOf[List[List[_u.Tree]]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = _build.SyntacticApplied.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Some[(Tree, List[List[Tree]])]]
    }
    val SyntacticAppliedType: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree = _build.SyntacticAppliedType.apply(tree.asInstanceOf[_u.Tree], targs.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree, List[Tree])] = _build.SyntacticAppliedType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, List[Tree])]]
    }
    val SyntacticAssign: SyntacticAssignExtractor = new SyntacticAssignExtractor {
      def apply(lhs: Tree, rhs: Tree): Tree = _build.SyntacticAssign.apply(lhs.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree, Tree)] = _build.SyntacticAssign.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, Tree)]]
    }
    val SyntacticBlock: SyntacticBlockExtractor = new SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree = _build.SyntacticBlock.apply(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[List[Tree]] = _build.SyntacticBlock.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[Tree]]]
    }
    val SyntacticClassDef: SyntacticClassDefExtractor = new SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                constrMods: Modifiers, vparamss: List[List[Tree]],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef =
        _build.SyntacticClassDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TypeName], tparams.asInstanceOf[List[_u.Tree]],
                                                 constrMods.asInstanceOf[_u.Modifiers], vparamss.asInstanceOf[List[List[_u.Tree]]],
                                                 earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[ClassDef]
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])] = _build.SyntacticClassDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]], List[Tree], List[Tree], ValDef, List[Tree])]]
    }
    val SyntacticDefDef: SyntacticDefDefExtractor = new SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree],
                vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef = _build.SyntacticDefDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tparams.asInstanceOf[List[_u.Tree]], vparamss.asInstanceOf[List[List[_u.Tree]]], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[DefDef]
      def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)] = _build.SyntacticDefDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)]]
    }
    val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor = new SyntacticEmptyTypeTreeExtractor {
      def apply(): TypeTree = _build.SyntacticEmptyTypeTree().asInstanceOf[TypeTree]
      def unapply(tt: TypeTree): Boolean = _build.SyntacticEmptyTypeTree.unapply(tt.asInstanceOf[_u.TypeTree])
    }
    val SyntacticFilter: SyntacticFilterExtractor = new SyntacticFilterExtractor {
      def apply(test: Tree): Tree = _build.SyntacticFilter.apply(test.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree)] = _build.SyntacticFilter.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree)]]
    }
    val SyntacticFor: SyntacticForExtractor = new SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree = _build.SyntacticFor.apply(enums.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(List[Tree], Tree)] = _build.SyntacticFor.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[Tree], Tree)]]
    }
    val SyntacticForYield: SyntacticForExtractor = new SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree = _build.SyntacticForYield.apply(enums.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(List[Tree], Tree)] = _build.SyntacticForYield.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[Tree], Tree)]]
    }
    val SyntacticFunction: SyntacticFunctionExtractor = new SyntacticFunctionExtractor {
      def apply(params: List[Tree], body: Tree): Function = _build.SyntacticFunction.apply(params.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[Function]
      def unapply(tree: Function): Option[(List[ValDef], Tree)] = _build.SyntacticFunction.unapply(tree.asInstanceOf[_u.Function]).asInstanceOf[Option[(List[ValDef], Tree)]]
    }
    val SyntacticFunctionType: SyntacticFunctionTypeExtractor = new SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree = _build.SyntacticFunctionType.apply(argtpes.asInstanceOf[List[_u.Tree]], restpe.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(List[Tree], Tree)] = _build.SyntacticFunctionType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[Tree], Tree)]]
    }
    val SyntacticIdent: SyntacticIdentExtractor = new SyntacticIdentExtractor {
      def apply(name: Name, isBackquoted: Boolean = false): Ident = _build.SyntacticIdent.apply(name.asInstanceOf[_u.Name], isBackquoted).asInstanceOf[Ident]
      def unapply(tree: Ident): Option[(Name, Boolean)] = _build.SyntacticIdent.unapply(tree.asInstanceOf[_u.Ident]).asInstanceOf[Option[(Name, Boolean)]]
    }
    val SyntacticImport: SyntacticImportExtractor = new SyntacticImportExtractor {
      def apply(expr: Tree, selectors: List[Tree]): Import = _build.SyntacticImport.apply(expr.asInstanceOf[_u.Tree], selectors.asInstanceOf[List[_u.Tree]]).asInstanceOf[Import]
      def unapply(imp: Import): Some[(Tree, List[Tree])] = _build.SyntacticImport.unapply(imp.asInstanceOf[_u.Import]).asInstanceOf[Some[(Tree, List[Tree])]]
    }
    val SyntacticMatch: SyntacticMatchExtractor = new SyntacticMatchExtractor {
      def apply(scrutinee: Tree, cases: List[Tree]): Match = _build.SyntacticMatch.apply(scrutinee.asInstanceOf[_u.Tree], cases.asInstanceOf[List[_u.Tree]]).asInstanceOf[Match]
      def unapply(tree: Match): Option[(Tree, List[CaseDef])] = _build.SyntacticMatch.unapply(tree.asInstanceOf[_u.Match]).asInstanceOf[Option[(Tree, List[CaseDef])]]
    }
    val SyntacticNew: SyntacticNewExtractor = new SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree = _build.SyntacticNew.apply(earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = _build.SyntacticNew.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[Tree], List[Tree], ValDef, List[Tree])]]
    }
    val SyntacticObjectDef: SyntacticObjectDefExtractor = new SyntacticObjectDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ModuleDef =
        _build.SyntacticObjectDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[ModuleDef]
      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])] =
        _build.SyntacticObjectDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])]]
    }
    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor = new SyntacticPackageObjectDefExtractor {
      def apply(name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): PackageDef =
        _build.SyntacticPackageObjectDef.apply(name.asInstanceOf[_u.TermName], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[PackageDef]
      def unapply(tree: Tree): Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])] =
        _build.SyntacticPackageObjectDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])]]
    }
    val SyntacticPartialFunction: SyntacticPartialFunctionExtractor = new SyntacticPartialFunctionExtractor {
      def apply(cases: List[Tree]): Match = _build.SyntacticPartialFunction.apply(cases.asInstanceOf[List[_u.Tree]]).asInstanceOf[Match]
      def unapply(tree: Match): Option[List[CaseDef]] = _build.SyntacticPartialFunction.unapply(tree.asInstanceOf[_u.Match]).asInstanceOf[Option[List[CaseDef]]]
    }
    val SyntacticPatDef: SyntacticPatDefExtractor = new SyntacticPatDefExtractor {
      def apply(mods: Modifiers, pat: Tree, tpt: Tree, rhs: Tree): List[ValDef] = _build.SyntacticPatDef.apply(mods.asInstanceOf[_u.Modifiers], pat.asInstanceOf[_u.Tree], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[List[ValDef]]
    }
    val SyntacticSelectTerm: SyntacticSelectTermExtractor = new SyntacticSelectTermExtractor {
      def apply(qual: Tree, name: TermName): Select = _build.SyntacticSelectTerm.apply(qual.asInstanceOf[_u.Tree], name.asInstanceOf[_u.TermName]).asInstanceOf[Select]
      def unapply(tree: Tree): Option[(Tree, TermName)] = _build.SyntacticSelectTerm.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, TermName)]]
    }
    val SyntacticSelectType: SyntacticSelectTypeExtractor = new SyntacticSelectTypeExtractor {
      def apply(qual: Tree, name: TypeName): Select = _build.SyntacticSelectType.apply(qual.asInstanceOf[_u.Tree], name.asInstanceOf[_u.TypeName]).asInstanceOf[Select]
      def unapply(tree: Tree): Option[(Tree, TypeName)] = _build.SyntacticSelectType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, TypeName)]]
    }
    val SyntacticTraitDef: SyntacticTraitDefExtractor = new SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef =
        _build.SyntacticTraitDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TypeName], tparams.asInstanceOf[List[_u.Tree]], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[ClassDef]
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])] =
        _build.SyntacticTraitDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])]]

    }
    val SyntacticTry: SyntacticTryExtractor = new SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree): Try = _build.SyntacticTry.apply(block.asInstanceOf[_u.Tree], catches.asInstanceOf[List[_u.Tree]], finalizer.asInstanceOf[_u.Tree]).asInstanceOf[Try]
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)] = _build.SyntacticTry.unapply(tree.asInstanceOf[_u.Try]).asInstanceOf[Option[(Tree, List[CaseDef], Tree)]]
    }
    val SyntacticTuple: SyntacticTupleExtractor = new SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = _build.SyntacticTuple.apply(args.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[List[Tree]] = _build.SyntacticTuple.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[Tree]]]
    }
    val SyntacticTupleType: SyntacticTupleExtractor = new SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree = _build.SyntacticTupleType.apply(args.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[List[Tree]] = _build.SyntacticTupleType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[Tree]]]
    }
    val SyntacticTypeApplied: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree = _build.SyntacticTypeApplied.apply(tree.asInstanceOf[_u.Tree], targs.asInstanceOf[List[_u.Tree]]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree, List[Tree])] = _build.SyntacticTypeApplied.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, List[Tree])]]
    }
    val SyntacticValDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = _build.SyntacticValDef(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[ValDef]
      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = _build.SyntacticValDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TermName, Tree, Tree)]]
    }
    val SyntacticValEq: SyntacticValEqExtractor = new SyntacticValEqExtractor {
      def apply(pat: Tree, rhs: Tree): Tree = _build.SyntacticValEq.apply(pat.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree, Tree)] = _build.SyntacticValEq.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, Tree)]]
    }
    val SyntacticValFrom: SyntacticValFromExtractor = new SyntacticValFromExtractor {
      def apply(pat: Tree, rhs: Tree): Tree = _build.SyntacticValFrom.apply(pat.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
      def unapply(tree: Tree): Option[(Tree, Tree)] = _build.SyntacticValFrom.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Tree, Tree)]]
    }
    val SyntacticVarDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = _build.SyntacticVarDef(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[ValDef]
      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = _build.SyntacticVarDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(Modifiers, TermName, Tree, Tree)]]
    }
    def ThisType(sym: Symbol): Type = _build.ThisType(sym.asInstanceOf[_u.Symbol]).asInstanceOf[Type]
    def TypeBounds(lo: Type,hi: Type): TypeBounds = _build.TypeBounds(lo.asInstanceOf[_u.Type], hi.asInstanceOf[_u.Type]).asInstanceOf[TypeBounds]
    def TypeRef(pre: Type,sym: Symbol,args: List[Type]): Type = _build.TypeRef(pre.asInstanceOf[_u.Type], sym.asInstanceOf[_u.Symbol], args.asInstanceOf[List[_u.Type]]).asInstanceOf[TypeRef]
    // def UnliftListElementwise[T](unliftable: Unliftable[T]): UnliftListElementwise[T] = _build.UnliftListElementwise(unliftable.asInstanceOf[_u.Unliftable[T]]).asInstanceOf[UnliftListElementwise[T]]
    // def UnliftListOfListsElementwise[T](unliftable: Unliftable[T]): UnliftListOfListsElementwise[T] = _build.UnliftListElementwise(unliftable.asInstanceOf[_u.Unliftable[T]]).asInstanceOf[UnliftListOfListsElementwise[T]]
    def freshTermName(prefix: String): TermName = _build.freshTermName(prefix).asInstanceOf[TermName]
    def freshTypeName(prefix: String): TypeName = _build.freshTypeName(prefix).asInstanceOf[TypeName]
    def mkAnnotation(trees: List[Tree]): List[Tree] = _build.mkAnnotation(trees.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[Tree]]
    def mkAnnotation(tree: Tree): Tree = _build.mkAnnotation(tree.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
    def mkEarlyDef(defns: List[Tree]): List[Tree] = _build.mkEarlyDef(defns.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[Tree]]
    def mkEarlyDef(defn: Tree): Tree = _build.mkEarlyDef(defn.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
    def mkIdent(sym: Symbol): Ident = _build.mkIdent(sym.asInstanceOf[_u.Symbol]).asInstanceOf[Ident]
    def mkPackageStat(stats: List[Tree]): List[Tree] = _build.mkPackageStat(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[Tree]]
    def mkPackageStat(stat: Tree): Tree = _build.mkPackageStat(stat.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
    def mkRefTree(qual: Tree,sym: Symbol): Tree = _build.mkRefTree(qual.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[Tree]
    def mkRefineStat(stats: List[Tree]): List[Tree] = _build.mkRefineStat(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[Tree]]
    def mkRefineStat(stat: Tree): Tree = _build.mkRefineStat(stat.asInstanceOf[_u.Tree]).asInstanceOf[Tree]
    def mkSelect(qualifier: Tree,sym: Symbol): Select = _build.mkSelect(qualifier.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[Select]
    def mkThis(sym: Symbol): Tree = _build.mkThis(sym.asInstanceOf[_u.Symbol]).asInstanceOf[Tree]
    def mkTypeTree(tp: Type): TypeTree = _build.mkTypeTree(tp.asInstanceOf[_u.Type]).asInstanceOf[TypeTree]
    def newFreeTerm(name: String,value: => Any,flags: FlagSet,origin: String): FreeTermSymbol = _build.newFreeTerm(name, value, flags.asInstanceOf[_u.FlagSet], origin).asInstanceOf[FreeTermSymbol]
    def newFreeType(name: String,flags: FlagSet,origin: String): FreeTypeSymbol = _build.newFreeType(name, flags.asInstanceOf[_u.FlagSet], origin).asInstanceOf[FreeTypeSymbol]
    def newNestedSymbol(owner: Symbol,name: Name,pos: Position,flags: FlagSet,isClass: Boolean): Symbol = _build.newNestedSymbol(owner.asInstanceOf[_u.Symbol], name.asInstanceOf[_u.Name], pos.asInstanceOf[_u.Position], flags.asInstanceOf[_u.FlagSet], isClass).asInstanceOf[Symbol]
    def newScopeWith(elems: Symbol*): Scope = _build.newScopeWith(elems.toList.asInstanceOf[List[_u.Symbol]]: _*).asInstanceOf[Scope]
    def selectOverloadedMethod(owner: Symbol,name: String,index: Int): MethodSymbol = _build.selectOverloadedMethod(owner.asInstanceOf[_u.Symbol], name, index).asInstanceOf[MethodSymbol]
    def selectTerm(owner: Symbol,name: String): TermSymbol = _build.selectTerm(owner.asInstanceOf[_u.Symbol], name).asInstanceOf[TermSymbol]
    def selectType(owner: Symbol,name: String): TypeSymbol = _build.selectType(owner.asInstanceOf[_u.Symbol], name).asInstanceOf[TypeSymbol]
    def setAnnotations[S <: Symbol](sym: S,annots: List[Annotation]): S = _build.setAnnotations(sym.asInstanceOf[_u.Symbol], annots.asInstanceOf[List[_u.Annotation]]).asInstanceOf[S]
    def setInfo[S <: Symbol](sym: S,tpe: Type): S = _build.setInfo(sym.asInstanceOf[_u.Symbol], tpe.asInstanceOf[_u.Type]).asInstanceOf[S]
    def setSymbol[T <: Tree](tree: T,sym: Symbol): T = _build.setSymbol(tree.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[T]
    def setType[T <: Tree](tree: T,tpe: Type): T = _build.setType(tree.asInstanceOf[_u.Tree], tpe.asInstanceOf[_u.Type]).asInstanceOf[T]
    def thisPrefix(sym: Symbol): Type = _build.thisPrefix(sym.asInstanceOf[_u.Symbol]).asInstanceOf[Type]
    def toStats(tree: Tree): List[Tree] = _build.toStats(tree.asInstanceOf[_u.Tree]).asInstanceOf[List[Tree]]
  }
}