package scala.quasiquotes

import scala.reflect.api._
import scala.reflect.internal.SymbolTable
import scala.reflect.ClassTag

object QuasiquoteCompat { def apply[U <: Universe with Singleton](u0: U): QuasiquoteCompat[u0.type] = new { val u: u0.type = u0 } with QuasiquoteCompat[u0.type] }
trait QuasiquoteCompat[U <: Universe with Singleton] {
  val u: U

  lazy val reificationSupport: build.type = build
  lazy val build: ReificationSupportApi = new ReificationSupportImpl

  // NOTE: this trait is copy/pasted from scala/scala@bcf24ec9ba
  // however the `CompatSupportApi with StandardLiftableApi with StandardUnliftableApi` part is custom to paradise 2.10.x
  trait ReificationSupportApi extends CompatSupportApi with StandardLiftableApi with StandardUnliftableApi {
    /** Selects type symbol with given simple name `name` from the defined members of `owner`.
     */
    def selectType(owner: U#Symbol, name: String): U#TypeSymbol

    /** Selects term symbol with given name and type from the defined members of prefix type
     */
    def selectTerm(owner: U#Symbol, name: String): U#TermSymbol

    /** Selects overloaded method symbol with given name and index
     */
    def selectOverloadedMethod(owner: U#Symbol, name: String, index: Int): U#MethodSymbol

    /** A fresh symbol with given name `name`, position `pos` and flags `flags` that has
     *  the current symbol as its owner.
     */
    def newNestedSymbol(owner: U#Symbol, name: U#Name, pos: U#Position, flags: U#FlagSet, isClass: Boolean): U#Symbol

    def newScopeWith(elems: U#Symbol*): U#Scope

    /** Create a fresh free term symbol.
     *  @param   name   the name of the free variable
     *  @param   value  the value of the free variable at runtime
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeTerm(name: String, value: => Any, flags: U#FlagSet = u.NoFlags, origin: String = null): U#FreeTermSymbol

    /** Create a fresh free type symbol.
     *  @param   name   the name of the free variable
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeType(name: String, flags: U#FlagSet = u.NoFlags, origin: String = null): U#FreeTypeSymbol

    /** Set symbol's type signature to given type.
     *  @return the symbol itself
     */
    def setInfo[S <: U#Symbol](sym: S, tpe: U#Type): S

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations[S <: U#Symbol](sym: S, annots: List[U#Annotation]): S

    def mkThis(sym: U#Symbol): U#Tree

    def mkSelect(qualifier: U#Tree, sym: U#Symbol): U#Select

    def mkIdent(sym: U#Symbol): U#Ident

    def mkTypeTree(tp: U#Type): U#TypeTree

    def ThisType(sym: U#Symbol): U#Type

    def SingleType(pre: U#Type, sym: U#Symbol): U#Type

    def SuperType(thistpe: U#Type, supertpe: U#Type): U#Type

    def ConstantType(value: U#Constant): U#ConstantType

    def TypeRef(pre: U#Type, sym: U#Symbol, args: List[U#Type]): U#Type

    def RefinedType(parents: List[U#Type], decls: U#Scope, typeSymbol: U#Symbol): U#RefinedType

    def ClassInfoType(parents: List[U#Type], decls: U#Scope, typeSymbol: U#Symbol): U#ClassInfoType

    def MethodType(params: List[U#Symbol], resultType: U#Type): U#MethodType

    def NullaryMethodType(resultType: U#Type): U#NullaryMethodType

    def PolyType(typeParams: List[U#Symbol], resultType: U#Type): U#PolyType

    def ExistentialType(quantified: List[U#Symbol], underlying: U#Type): U#ExistentialType

    def AnnotatedType(annotations: List[U#Annotation], underlying: U#Type, selfsym: U#Symbol): U#AnnotatedType

    def TypeBounds(lo: U#Type, hi: U#Type): U#TypeBounds

    def BoundedWildcardType(bounds: U#TypeBounds): U#BoundedWildcardType

    def thisPrefix(sym: U#Symbol): U#Type

    def setType[T <: U#Tree](tree: T, tpe: U#Type): T

    def setSymbol[T <: U#Tree](tree: T, sym: U#Symbol): T

    def toStats(tree: U#Tree): List[U#Tree]

    def mkAnnotation(tree: U#Tree): U#Tree

    def mkAnnotation(trees: List[U#Tree]): List[U#Tree]

    def mkRefineStat(stat: U#Tree): U#Tree

    def mkRefineStat(stats: List[U#Tree]): List[U#Tree]

    def mkPackageStat(stat: U#Tree): U#Tree

    def mkPackageStat(stats: List[U#Tree]): List[U#Tree]

    def mkEarlyDef(defn: U#Tree): U#Tree

    def mkEarlyDef(defns: List[U#Tree]): List[U#Tree]

    def mkRefTree(qual: U#Tree, sym: U#Symbol): U#Tree

    def freshTermName(prefix: String): U#TermName

    def freshTypeName(prefix: String): U#TypeName

    val ImplicitParams: ImplicitParamsExtractor

    trait ImplicitParamsExtractor {
      def apply(paramss: List[List[U#Tree]], implparams: List[U#Tree]): List[List[U#Tree]]
      def unapply(vparamss: List[List[U#ValDef]]): Some[(List[List[U#ValDef]], List[U#ValDef])]
    }

    val ScalaDot: ScalaDotExtractor

    trait ScalaDotExtractor {
      def apply(name: U#Name): U#Tree
      def unapply(tree: U#Tree): Option[U#Name]
    }

    val FlagsRepr: FlagsReprExtractor

    trait FlagsReprExtractor {
      def apply(value: Long): U#FlagSet
      def unapply(flags: Long): Some[Long]
    }

    val SyntacticTypeApplied: SyntacticTypeAppliedExtractor
    val SyntacticAppliedType: SyntacticTypeAppliedExtractor

    trait SyntacticTypeAppliedExtractor {
      def apply(tree: U#Tree, targs: List[U#Tree]): U#Tree
      def unapply(tree: U#Tree): Option[(U#Tree, List[U#Tree])]
    }

    val SyntacticApplied: SyntacticAppliedExtractor

    trait SyntacticAppliedExtractor {
      def apply(tree: U#Tree, argss: List[List[U#Tree]]): U#Tree
      def unapply(tree: U#Tree): Some[(U#Tree, List[List[U#Tree]])]
    }

    val SyntacticClassDef: SyntacticClassDefExtractor

    trait SyntacticClassDefExtractor {
      def apply(mods: U#Modifiers, name: U#TypeName, tparams: List[U#Tree],
                constrMods: U#Modifiers, vparamss: List[List[U#Tree]],
                earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ClassDef
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TypeName, List[U#TypeDef], U#Modifiers, List[List[U#ValDef]],
                                       List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]
    }

    val SyntacticTraitDef: SyntacticTraitDefExtractor

    trait SyntacticTraitDefExtractor {
      def apply(mods: U#Modifiers, name: U#TypeName, tparams: List[U#Tree],
                earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ClassDef
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TypeName, List[U#TypeDef],
                                       List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]
    }

    val SyntacticObjectDef: SyntacticObjectDefExtractor

    trait SyntacticObjectDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, earlyDefs: List[U#Tree],
                parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ModuleDef
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]
    }

    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor

    trait SyntacticPackageObjectDefExtractor {
      def apply(name: U#TermName, earlyDefs: List[U#Tree],
                parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#PackageDef
      def unapply(tree: U#Tree): Option[(U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]
    }

    val SyntacticTuple: SyntacticTupleExtractor
    val SyntacticTupleType: SyntacticTupleExtractor

    trait SyntacticTupleExtractor {
      def apply(args: List[U#Tree]): U#Tree
      def unapply(tree: U#Tree): Option[List[U#Tree]]
    }

    val SyntacticBlock: SyntacticBlockExtractor

    trait SyntacticBlockExtractor {
      def apply(stats: List[U#Tree]): U#Tree
      def unapply(tree: U#Tree): Option[List[U#Tree]]
    }

    val SyntacticNew: SyntacticNewExtractor

    trait SyntacticNewExtractor {
      def apply(earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#Tree
      def unapply(tree: U#Tree): Option[(List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]
    }

    val SyntacticFunctionType: SyntacticFunctionTypeExtractor

    trait SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[U#Tree], restpe: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(List[U#Tree], U#Tree)]
    }

    val SyntacticFunction: SyntacticFunctionExtractor

    trait SyntacticFunctionExtractor {
      def apply(params: List[U#Tree], body: U#Tree): U#Function

      def unapply(tree: U#Function): Option[(List[U#ValDef], U#Tree)]
    }

    val SyntacticDefDef: SyntacticDefDefExtractor

    trait SyntacticDefDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, tparams: List[U#Tree],
                vparamss: List[List[U#Tree]], tpt: U#Tree, rhs: U#Tree): U#DefDef

      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, List[U#TypeDef], List[List[U#ValDef]], U#Tree, U#Tree)]
    }

    val SyntacticValDef: SyntacticValDefExtractor
    val SyntacticVarDef: SyntacticValDefExtractor

    trait SyntacticValDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, tpt: U#Tree, rhs: U#Tree): U#ValDef
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, U#Tree, U#Tree)]
    }

    val SyntacticPatDef: SyntacticPatDefExtractor

    trait SyntacticPatDefExtractor {
      def apply(mods: U#Modifiers, pat: U#Tree, tpt: U#Tree, rhs: U#Tree): List[U#ValDef]
    }

    val SyntacticAssign: SyntacticAssignExtractor

    trait SyntacticAssignExtractor {
      def apply(lhs: U#Tree, rhs: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)]
    }

    val SyntacticValFrom: SyntacticValFromExtractor

    trait SyntacticValFromExtractor {
      def apply(pat: U#Tree, rhs: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)]
    }

    val SyntacticValEq: SyntacticValEqExtractor

    trait SyntacticValEqExtractor {
      def apply(pat: U#Tree, rhs: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)]
    }

    val SyntacticFilter: SyntacticFilterExtractor

    trait SyntacticFilterExtractor {
      def apply(test: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(U#Tree)]
    }

    val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor

    trait SyntacticEmptyTypeTreeExtractor {
      def apply(): U#TypeTree
      def unapply(tt: U#TypeTree): Boolean
    }

    val SyntacticFor: SyntacticForExtractor
    val SyntacticForYield: SyntacticForExtractor

    trait SyntacticForExtractor {
      def apply(enums: List[U#Tree], body: U#Tree): U#Tree
      def unapply(tree: U#Tree): Option[(List[U#Tree], U#Tree)]
    }

    // def UnliftListElementwise[T](unliftable: U#Unliftable[T]): UnliftListElementwise[T]
    // trait UnliftListElementwise[T] {
    //   def unapply(lst: List[U#Tree]): Option[List[T]]
    // }

    // def UnliftListOfListsElementwise[T](unliftable: U#Unliftable[T]): UnliftListOfListsElementwise[T]
    // trait UnliftListOfListsElementwise[T] {
    //   def unapply(lst: List[List[U#Tree]]): Option[List[List[T]]]
    // }

    val SyntacticPartialFunction: SyntacticPartialFunctionExtractor
    trait SyntacticPartialFunctionExtractor {
      def apply(cases: List[U#Tree]): U#Match
      def unapply(tree: U#Match): Option[List[U#CaseDef]]
    }

    val SyntacticMatch: SyntacticMatchExtractor
    trait SyntacticMatchExtractor {
      def apply(scrutinee: U#Tree, cases: List[U#Tree]): U#Match
      def unapply(tree: U#Match): Option[(U#Tree, List[U#CaseDef])]
    }

    val SyntacticTry: SyntacticTryExtractor
    trait SyntacticTryExtractor {
      def apply(block: U#Tree, catches: List[U#Tree], finalizer: U#Tree): U#Try
      def unapply(tree: U#Try): Option[(U#Tree, List[U#CaseDef], U#Tree)]
    }

    val SyntacticIdent: SyntacticIdentExtractor
    trait SyntacticIdentExtractor {
      def apply(name: U#Name, isBackquoted: Boolean = false): U#Ident
      def unapply(tree: U#Ident): Option[(U#Name, Boolean)]
    }

    val SyntacticImport: SyntacticImportExtractor
    trait SyntacticImportExtractor {
      def apply(expr: U#Tree, selectors: List[U#Tree]): U#Import
      def unapply(imp: U#Import): Some[(U#Tree, List[U#Tree])]
    }

    val SyntacticSelectType: SyntacticSelectTypeExtractor
    trait SyntacticSelectTypeExtractor {
      def apply(qual: U#Tree, name: U#TypeName): U#Select
      def unapply(tree: U#Tree): Option[(U#Tree, U#TypeName)]
    }

    val SyntacticSelectTerm: SyntacticSelectTermExtractor
    trait SyntacticSelectTermExtractor {
      def apply(qual: U#Tree, name: U#TermName): U#Select
      def unapply(tree: U#Tree): Option[(U#Tree, U#TermName)]
    }
  }

  trait CompatSupportApi {
    object TermName {
      def apply(s: String): U#TermName = u.newTermName(s)
      def unapply(name: U#TermName): Some[String] = Some(name.toString)
    }

    object TypeName {
      def apply(s: String): U#TypeName = u.newTypeName(s)
      def unapply(name: U#TypeName): Some[String] = Some(name.toString)
    }

    object Modifiers {
      def apply(flags: U#FlagSet, privateWithin: U#Name = TermName(""), annotations: List[U#Tree] = Nil): U#Modifiers =
        u.Modifiers(flags.asInstanceOf[u.FlagSet], privateWithin.asInstanceOf[u.Name], annotations.asInstanceOf[List[u.Tree]])
      def unapply(mods: U#Modifiers): Some[(U#FlagSet, U#Name, List[U#Tree])] =
        Some((mods.flags, mods.privateWithin, mods.annotations))
    }

    object EmptyValDefLike {
      def apply(): U#ValDef = u.emptyValDef
      def unapply(tree: U#Tree): Boolean = tree eq u.emptyValDef
    }
  }

  trait StandardLiftableApi {
    implicit def liftByte[T <: Byte]: U#Liftable[T]
    implicit def liftShort[T <: Short]: U#Liftable[T]
    implicit def liftChar[T <: Char]: U#Liftable[T]
    implicit def liftInt[T <: Int]: U#Liftable[T]
    implicit def liftLong[T <: Long]: U#Liftable[T]
    implicit def liftFloat[T <: Float]: U#Liftable[T]
    implicit def liftDouble[T <: Double]: U#Liftable[T]
    implicit def liftBoolean[T <: Boolean]: U#Liftable[T]
    implicit def liftUnit: U#Liftable[Unit]
    implicit def liftString[T <: String]: U#Liftable[T]
    implicit def liftScalaSymbol: U#Liftable[scala.Symbol]
    implicit def liftTree[T <: U#Tree]: U#Liftable[T]
    implicit def liftName[T <: U#Name]: U#Liftable[T]
    implicit def liftExpr[T <: U#Expr[_]]: U#Liftable[T]
    implicit def liftType[T <: U#Type]: U#Liftable[T]
    implicit def liftTypeTag[T <: U#WeakTypeTag[_]]: U#Liftable[T]
    implicit def liftConstant[T <: U#Constant]: U#Liftable[T]
    implicit def liftArray[T: U#Liftable]: U#Liftable[Array[T]]
    implicit def liftVector[T: U#Liftable]: U#Liftable[Vector[T]]
    implicit def liftList[T: U#Liftable]: U#Liftable[List[T]]
    implicit def liftNil: U#Liftable[Nil.type]
    implicit def liftMap[K: U#Liftable, V: U#Liftable]: U#Liftable[Map[K, V]]
    implicit def liftSet[T: U#Liftable]: U#Liftable[Set[T]]
    implicit def liftSome[T: U#Liftable]: U#Liftable[Some[T]]
    implicit def liftNone: U#Liftable[None.type]
    implicit def liftOption[T: U#Liftable]: U#Liftable[Option[T]]
    implicit def liftLeft[L: U#Liftable, R]: U#Liftable[Left[L, R]]
    implicit def liftRight[L, R: U#Liftable]: U#Liftable[Right[L, R]]
    implicit def liftEither[L: U#Liftable, R: U#Liftable]: U#Liftable[Either[L, R]]
    implicit def liftTuple2[T1, T2](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2]): U#Liftable[Tuple2[T1, T2]]
    implicit def liftTuple3[T1, T2, T3](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3]): U#Liftable[Tuple3[T1, T2, T3]]
    implicit def liftTuple4[T1, T2, T3, T4](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4]): U#Liftable[Tuple4[T1, T2, T3, T4]]
    implicit def liftTuple5[T1, T2, T3, T4, T5](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5]): U#Liftable[Tuple5[T1, T2, T3, T4, T5]]
    implicit def liftTuple6[T1, T2, T3, T4, T5, T6](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6]): U#Liftable[Tuple6[T1, T2, T3, T4, T5, T6]]
    implicit def liftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7]): U#Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]]
    implicit def liftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8]): U#Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]
    implicit def liftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9]): U#Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]
    implicit def liftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10]): U#Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
    implicit def liftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11]): U#Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
    implicit def liftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12]): U#Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
    implicit def liftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13]): U#Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
    implicit def liftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14]): U#Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
    implicit def liftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15]): U#Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
    implicit def liftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16]): U#Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
    implicit def liftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17]): U#Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
    implicit def liftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18]): U#Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]
    implicit def liftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19]): U#Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]
    implicit def liftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20]): U#Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]
    implicit def liftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20], liftT21: U#Liftable[T21]): U#Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]
    implicit def liftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20], liftT21: U#Liftable[T21], liftT22: U#Liftable[T22]): U#Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]
  }

  trait StandardUnliftableApi {
    implicit def unliftByte: U#Unliftable[Byte]
    implicit def unliftShort: U#Unliftable[Short]
    implicit def unliftChar: U#Unliftable[Char]
    implicit def unliftInt: U#Unliftable[Int]
    implicit def unliftLong: U#Unliftable[Long]
    implicit def unliftFloat: U#Unliftable[Float]
    implicit def unliftDouble: U#Unliftable[Double]
    implicit def unliftBoolean: U#Unliftable[Boolean]
    implicit def unliftUnit: U#Unliftable[Unit]
    implicit def unliftString: U#Unliftable[String]
    implicit def unliftScalaSymbol: U#Unliftable[scala.Symbol]
    implicit def unliftName[T <: U#Name : ClassTag]: U#Unliftable[T]
    implicit def unliftType: U#Unliftable[U#Type]
    implicit def unliftConstant: U#Unliftable[U#Constant]
    implicit def unliftTuple2[T1, T2](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2]): U#Unliftable[Tuple2[T1, T2]]
    implicit def unliftTuple3[T1, T2, T3](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3]): U#Unliftable[Tuple3[T1, T2, T3]]
    implicit def unliftTuple4[T1, T2, T3, T4](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4]): U#Unliftable[Tuple4[T1, T2, T3, T4]]
    implicit def unliftTuple5[T1, T2, T3, T4, T5](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5]): U#Unliftable[Tuple5[T1, T2, T3, T4, T5]]
    implicit def unliftTuple6[T1, T2, T3, T4, T5, T6](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6]): U#Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]]
    implicit def unliftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7]): U#Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]]
    implicit def unliftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8]): U#Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]
    implicit def unliftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9]): U#Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]
    implicit def unliftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10]): U#Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
    implicit def unliftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11]): U#Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
    implicit def unliftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12]): U#Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
    implicit def unliftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13]): U#Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
    implicit def unliftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14]): U#Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
    implicit def unliftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15]): U#Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
    implicit def unliftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16]): U#Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
    implicit def unliftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17]): U#Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
    implicit def unliftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18]): U#Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]
    implicit def unliftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19]): U#Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]
    implicit def unliftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20]): U#Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]
    implicit def unliftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20], unliftT21: U#Unliftable[T21]): U#Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]
    implicit def unliftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20], unliftT21: U#Unliftable[T21], unliftT22: U#Unliftable[T22]): U#Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]
  }

  // TODO: making these fields private will crash at runtime with IllegalAccessError
  val _u: SymbolTable = u.asInstanceOf[SymbolTable]
  val _build = new { val global: _u.type = _u } with ReificationSupport

  class ReificationSupportImpl extends ReificationSupportApi with CompatSupportApi with StandardLiftableApi with StandardLiftableImpl with StandardUnliftableApi with StandardUnliftableImpl {
    import u._, definitions._, Flag._
    def AnnotatedType(annotations: List[U#Annotation],underlying: U#Type,selfSym: U#Symbol): AnnotatedType = _build.AnnotatedType(annotations.asInstanceOf[List[_u.Annotation]], underlying.asInstanceOf[_u.Type], selfSym.asInstanceOf[_u.Symbol]).asInstanceOf[AnnotatedType]
    def BoundedWildcardType(bounds: U#TypeBounds): BoundedWildcardType = _build.BoundedWildcardType(bounds.asInstanceOf[_u.TypeBounds]).asInstanceOf[BoundedWildcardType]
    def ClassInfoType(parents: List[U#Type],decls: U#Scope,typeSymbol: U#Symbol): ClassInfoType = _build.ClassInfoType(parents.asInstanceOf[List[_u.Type]], decls.asInstanceOf[_u.Scope], typeSymbol.asInstanceOf[_u.Symbol]).asInstanceOf[ClassInfoType]
    def ConstantType(value: U#Constant): ConstantType = _build.ConstantType(value.asInstanceOf[_u.Constant]).asInstanceOf[ConstantType]
    def ExistentialType(quantified: List[U#Symbol],underlying: U#Type): ExistentialType = _build.ExistentialType(quantified.asInstanceOf[List[_u.Symbol]], underlying.asInstanceOf[_u.Type]).asInstanceOf[ExistentialType]
    val FlagsRepr: FlagsReprExtractor = new FlagsReprExtractor {
      def apply(value: Long): U#FlagSet = _build.FlagsRepr.apply(value).asInstanceOf[U#FlagSet]
      def unapply(flags: Long): Some[Long] = _build.FlagsRepr.unapply(flags)
    }
    val ImplicitParams: ImplicitParamsExtractor = new ImplicitParamsExtractor {
      def apply(paramss: List[List[U#Tree]], implparams: List[U#Tree]): List[List[U#Tree]] = _build.ImplicitParams.apply(paramss.asInstanceOf[List[List[_u.Tree]]], implparams.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[List[U#Tree]]]
      def unapply(vparamss: List[List[U#ValDef]]): Some[(List[List[U#ValDef]], List[U#ValDef])] = _build.ImplicitParams.unapply(vparamss.asInstanceOf[List[List[_u.ValDef]]]).asInstanceOf[Some[(List[List[U#ValDef]], List[U#ValDef])]]
    }
    def MethodType(params: List[U#Symbol],resultType: U#Type): MethodType = _build.MethodType(params.asInstanceOf[List[_u.Symbol]], resultType.asInstanceOf[_u.Type]).asInstanceOf[MethodType]
    def NullaryMethodType(resultType: U#Type): NullaryMethodType = _build.NullaryMethodType(resultType.asInstanceOf[_u.Type]).asInstanceOf[NullaryMethodType]
    def PolyType(typeParams: List[U#Symbol],resultType: U#Type): PolyType = _build.PolyType(typeParams.asInstanceOf[List[_u.Symbol]], resultType.asInstanceOf[_u.Type]).asInstanceOf[PolyType]
    def RefinedType(parents: List[U#Type],decls: U#Scope,typeSymbol: U#Symbol): RefinedType = _build.RefinedType(parents.asInstanceOf[List[_u.Type]], decls.asInstanceOf[_u.Scope], typeSymbol.asInstanceOf[_u.Symbol]).asInstanceOf[RefinedType]
    val ScalaDot: ScalaDotExtractor = new ScalaDotExtractor {
      def apply(name: U#Name): U#Tree = _build.ScalaDot.apply(name.asInstanceOf[_u.Name]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[U#Name] = _build.ScalaDot.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[U#Name]]
    }
    def SingleType(pre: U#Type,sym: U#Symbol): U#Type = _build.SingleType(pre.asInstanceOf[_u.Type], sym.asInstanceOf[_u.Symbol]).asInstanceOf[SingleType]
    def SuperType(thistpe: U#Type,supertpe: U#Type): U#Type = _build.SuperType(thistpe.asInstanceOf[_u.Type], supertpe.asInstanceOf[_u.Type]).asInstanceOf[SuperType]
    val SyntacticApplied: SyntacticAppliedExtractor = new SyntacticAppliedExtractor {
      def apply(tree: U#Tree, argss: List[List[U#Tree]]): U#Tree = _build.SyntacticApplied.apply(tree.asInstanceOf[_u.Tree], argss.asInstanceOf[List[List[_u.Tree]]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Some[(U#Tree, List[List[U#Tree]])] = _build.SyntacticApplied.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Some[(U#Tree, List[List[U#Tree]])]]
    }
    val SyntacticAppliedType: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
      def apply(tree: U#Tree, targs: List[U#Tree]): U#Tree = _build.SyntacticAppliedType.apply(tree.asInstanceOf[_u.Tree], targs.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree, List[U#Tree])] = _build.SyntacticAppliedType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, List[U#Tree])]]
    }
    val SyntacticAssign: SyntacticAssignExtractor = new SyntacticAssignExtractor {
      def apply(lhs: U#Tree, rhs: U#Tree): U#Tree = _build.SyntacticAssign.apply(lhs.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)] = _build.SyntacticAssign.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, U#Tree)]]
    }
    val SyntacticBlock: SyntacticBlockExtractor = new SyntacticBlockExtractor {
      def apply(stats: List[U#Tree]): U#Tree = _build.SyntacticBlock.apply(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[List[U#Tree]] = _build.SyntacticBlock.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[U#Tree]]]
    }
    val SyntacticClassDef: SyntacticClassDefExtractor = new SyntacticClassDefExtractor {
      def apply(mods: U#Modifiers, name: U#TypeName, tparams: List[U#Tree],
                constrMods: U#Modifiers, vparamss: List[List[U#Tree]],
                earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ClassDef =
        _build.SyntacticClassDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TypeName], tparams.asInstanceOf[List[_u.Tree]],
                                                 constrMods.asInstanceOf[_u.Modifiers], vparamss.asInstanceOf[List[List[_u.Tree]]],
                                                 earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#ClassDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TypeName, List[U#TypeDef], U#Modifiers, List[List[U#ValDef]],
                                       List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])] = _build.SyntacticClassDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TypeName, List[U#TypeDef], U#Modifiers, List[List[U#ValDef]], List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]]
    }
    val SyntacticDefDef: SyntacticDefDefExtractor = new SyntacticDefDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, tparams: List[U#Tree],
                vparamss: List[List[U#Tree]], tpt: U#Tree, rhs: U#Tree): DefDef = _build.SyntacticDefDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tparams.asInstanceOf[List[_u.Tree]], vparamss.asInstanceOf[List[List[_u.Tree]]], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[DefDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, List[U#TypeDef], List[List[U#ValDef]], U#Tree, U#Tree)] = _build.SyntacticDefDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TermName, List[U#TypeDef], List[List[U#ValDef]], U#Tree, U#Tree)]]
    }
    val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor = new SyntacticEmptyTypeTreeExtractor {
      def apply(): TypeTree = _build.SyntacticEmptyTypeTree().asInstanceOf[TypeTree]
      def unapply(tt: U#TypeTree): Boolean = _build.SyntacticEmptyTypeTree.unapply(tt.asInstanceOf[_u.TypeTree])
    }
    val SyntacticFilter: SyntacticFilterExtractor = new SyntacticFilterExtractor {
      def apply(test: U#Tree): U#Tree = _build.SyntacticFilter.apply(test.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree)] = _build.SyntacticFilter.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree)]]
    }
    val SyntacticFor: SyntacticForExtractor = new SyntacticForExtractor {
      def apply(enums: List[U#Tree], body: U#Tree): U#Tree = _build.SyntacticFor.apply(enums.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(List[U#Tree], U#Tree)] = _build.SyntacticFor.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[U#Tree], U#Tree)]]
    }
    val SyntacticForYield: SyntacticForExtractor = new SyntacticForExtractor {
      def apply(enums: List[U#Tree], body: U#Tree): U#Tree = _build.SyntacticForYield.apply(enums.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(List[U#Tree], U#Tree)] = _build.SyntacticForYield.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[U#Tree], U#Tree)]]
    }
    val SyntacticFunction: SyntacticFunctionExtractor = new SyntacticFunctionExtractor {
      def apply(params: List[U#Tree], body: U#Tree): Function = _build.SyntacticFunction.apply(params.asInstanceOf[List[_u.Tree]], body.asInstanceOf[_u.Tree]).asInstanceOf[Function]
      def unapply(tree: U#Function): Option[(List[U#ValDef], U#Tree)] = _build.SyntacticFunction.unapply(tree.asInstanceOf[_u.Function]).asInstanceOf[Option[(List[U#ValDef], U#Tree)]]
    }
    val SyntacticFunctionType: SyntacticFunctionTypeExtractor = new SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[U#Tree], restpe: U#Tree): U#Tree = _build.SyntacticFunctionType.apply(argtpes.asInstanceOf[List[_u.Tree]], restpe.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(List[U#Tree], U#Tree)] = _build.SyntacticFunctionType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[U#Tree], U#Tree)]]
    }
    val SyntacticIdent: SyntacticIdentExtractor = new SyntacticIdentExtractor {
      def apply(name: U#Name, isBackquoted: Boolean = false): Ident = _build.SyntacticIdent.apply(name.asInstanceOf[_u.Name], isBackquoted).asInstanceOf[Ident]
      def unapply(tree: U#Ident): Option[(U#Name, Boolean)] = _build.SyntacticIdent.unapply(tree.asInstanceOf[_u.Ident]).asInstanceOf[Option[(U#Name, Boolean)]]
    }
    val SyntacticImport: SyntacticImportExtractor = new SyntacticImportExtractor {
      def apply(expr: U#Tree, selectors: List[U#Tree]): Import = _build.SyntacticImport.apply(expr.asInstanceOf[_u.Tree], selectors.asInstanceOf[List[_u.Tree]]).asInstanceOf[Import]
      def unapply(imp: U#Import): Some[(U#Tree, List[U#Tree])] = _build.SyntacticImport.unapply(imp.asInstanceOf[_u.Import]).asInstanceOf[Some[(U#Tree, List[U#Tree])]]
    }
    val SyntacticMatch: SyntacticMatchExtractor = new SyntacticMatchExtractor {
      def apply(scrutinee: U#Tree, cases: List[U#Tree]): Match = _build.SyntacticMatch.apply(scrutinee.asInstanceOf[_u.Tree], cases.asInstanceOf[List[_u.Tree]]).asInstanceOf[Match]
      def unapply(tree: U#Match): Option[(U#Tree, List[CaseDef])] = _build.SyntacticMatch.unapply(tree.asInstanceOf[_u.Match]).asInstanceOf[Option[(U#Tree, List[CaseDef])]]
    }
    val SyntacticNew: SyntacticNewExtractor = new SyntacticNewExtractor {
      def apply(earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#Tree = _build.SyntacticNew.apply(earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])] = _build.SyntacticNew.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]]
    }
    val SyntacticObjectDef: SyntacticObjectDefExtractor = new SyntacticObjectDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, earlyDefs: List[U#Tree],
                parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ModuleDef =
        _build.SyntacticObjectDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#ModuleDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])] =
        _build.SyntacticObjectDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]]
    }
    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor = new SyntacticPackageObjectDefExtractor {
      def apply(name: U#TermName, earlyDefs: List[U#Tree],
                parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): PackageDef =
        _build.SyntacticPackageObjectDef.apply(name.asInstanceOf[_u.TermName], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[PackageDef]
      def unapply(tree: U#Tree): Option[(U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])] =
        _build.SyntacticPackageObjectDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#TermName, List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]]
    }
    val SyntacticPartialFunction: SyntacticPartialFunctionExtractor = new SyntacticPartialFunctionExtractor {
      def apply(cases: List[U#Tree]): Match = _build.SyntacticPartialFunction.apply(cases.asInstanceOf[List[_u.Tree]]).asInstanceOf[Match]
      def unapply(tree: U#Match): Option[List[CaseDef]] = _build.SyntacticPartialFunction.unapply(tree.asInstanceOf[_u.Match]).asInstanceOf[Option[List[CaseDef]]]
    }
    val SyntacticPatDef: SyntacticPatDefExtractor = new SyntacticPatDefExtractor {
      def apply(mods: U#Modifiers, pat: U#Tree, tpt: U#Tree, rhs: U#Tree): List[U#ValDef] = _build.SyntacticPatDef.apply(mods.asInstanceOf[_u.Modifiers], pat.asInstanceOf[_u.Tree], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[List[U#ValDef]]
    }
    val SyntacticSelectTerm: SyntacticSelectTermExtractor = new SyntacticSelectTermExtractor {
      def apply(qual: U#Tree, name: U#TermName): Select = _build.SyntacticSelectTerm.apply(qual.asInstanceOf[_u.Tree], name.asInstanceOf[_u.TermName]).asInstanceOf[Select]
      def unapply(tree: U#Tree): Option[(U#Tree, U#TermName)] = _build.SyntacticSelectTerm.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, U#TermName)]]
    }
    val SyntacticSelectType: SyntacticSelectTypeExtractor = new SyntacticSelectTypeExtractor {
      def apply(qual: U#Tree, name: U#TypeName): Select = _build.SyntacticSelectType.apply(qual.asInstanceOf[_u.Tree], name.asInstanceOf[_u.TypeName]).asInstanceOf[Select]
      def unapply(tree: U#Tree): Option[(U#Tree, U#TypeName)] = _build.SyntacticSelectType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, U#TypeName)]]
    }
    val SyntacticTraitDef: SyntacticTraitDefExtractor = new SyntacticTraitDefExtractor {
      def apply(mods: U#Modifiers, name: U#TypeName, tparams: List[U#Tree],
                earlyDefs: List[U#Tree], parents: List[U#Tree], selfType: U#Tree, body: List[U#Tree]): U#ClassDef =
        _build.SyntacticTraitDef.apply(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TypeName], tparams.asInstanceOf[List[_u.Tree]], earlyDefs.asInstanceOf[List[_u.Tree]], parents.asInstanceOf[List[_u.Tree]], selfType.asInstanceOf[_u.Tree], body.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#ClassDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TypeName, List[U#TypeDef],
                                       List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])] =
        _build.SyntacticTraitDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TypeName, List[U#TypeDef],
                                       List[U#Tree], List[U#Tree], U#ValDef, List[U#Tree])]]

    }
    val SyntacticTry: SyntacticTryExtractor = new SyntacticTryExtractor {
      def apply(block: U#Tree, catches: List[U#Tree], finalizer: U#Tree): Try = _build.SyntacticTry.apply(block.asInstanceOf[_u.Tree], catches.asInstanceOf[List[_u.Tree]], finalizer.asInstanceOf[_u.Tree]).asInstanceOf[Try]
      def unapply(tree: U#Try): Option[(U#Tree, List[CaseDef], U#Tree)] = _build.SyntacticTry.unapply(tree.asInstanceOf[_u.Try]).asInstanceOf[Option[(U#Tree, List[CaseDef], U#Tree)]]
    }
    val SyntacticTuple: SyntacticTupleExtractor = new SyntacticTupleExtractor {
      def apply(args: List[U#Tree]): U#Tree = _build.SyntacticTuple.apply(args.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[List[U#Tree]] = _build.SyntacticTuple.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[U#Tree]]]
    }
    val SyntacticTupleType: SyntacticTupleExtractor = new SyntacticTupleExtractor {
      def apply(args: List[U#Tree]): U#Tree = _build.SyntacticTupleType.apply(args.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[List[U#Tree]] = _build.SyntacticTupleType.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[List[U#Tree]]]
    }
    val SyntacticTypeApplied: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
      def apply(tree: U#Tree, targs: List[U#Tree]): U#Tree = _build.SyntacticTypeApplied.apply(tree.asInstanceOf[_u.Tree], targs.asInstanceOf[List[_u.Tree]]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree, List[U#Tree])] = _build.SyntacticTypeApplied.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, List[U#Tree])]]
    }
    val SyntacticValDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, tpt: U#Tree, rhs: U#Tree): U#ValDef = _build.SyntacticValDef(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[U#ValDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, U#Tree, U#Tree)] = _build.SyntacticValDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TermName, U#Tree, U#Tree)]]
    }
    val SyntacticValEq: SyntacticValEqExtractor = new SyntacticValEqExtractor {
      def apply(pat: U#Tree, rhs: U#Tree): U#Tree = _build.SyntacticValEq.apply(pat.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)] = _build.SyntacticValEq.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, U#Tree)]]
    }
    val SyntacticValFrom: SyntacticValFromExtractor = new SyntacticValFromExtractor {
      def apply(pat: U#Tree, rhs: U#Tree): U#Tree = _build.SyntacticValFrom.apply(pat.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
      def unapply(tree: U#Tree): Option[(U#Tree, U#Tree)] = _build.SyntacticValFrom.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Tree, U#Tree)]]
    }
    val SyntacticVarDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
      def apply(mods: U#Modifiers, name: U#TermName, tpt: U#Tree, rhs: U#Tree): U#ValDef = _build.SyntacticVarDef(mods.asInstanceOf[_u.Modifiers], name.asInstanceOf[_u.TermName], tpt.asInstanceOf[_u.Tree], rhs.asInstanceOf[_u.Tree]).asInstanceOf[U#ValDef]
      def unapply(tree: U#Tree): Option[(U#Modifiers, U#TermName, U#Tree, U#Tree)] = _build.SyntacticVarDef.unapply(tree.asInstanceOf[_u.Tree]).asInstanceOf[Option[(U#Modifiers, U#TermName, U#Tree, U#Tree)]]
    }
    def ThisType(sym: U#Symbol): U#Type = _build.ThisType(sym.asInstanceOf[_u.Symbol]).asInstanceOf[U#Type]
    def TypeBounds(lo: U#Type,hi: U#Type): TypeBounds = _build.TypeBounds(lo.asInstanceOf[_u.Type], hi.asInstanceOf[_u.Type]).asInstanceOf[TypeBounds]
    def TypeRef(pre: U#Type,sym: U#Symbol,args: List[U#Type]): U#Type = _build.TypeRef(pre.asInstanceOf[_u.Type], sym.asInstanceOf[_u.Symbol], args.asInstanceOf[List[_u.Type]]).asInstanceOf[TypeRef]
    // def UnliftListElementwise[T](unliftable: U#Unliftable[T]): UnliftListElementwise[T] = _build.UnliftListElementwise(unliftable.asInstanceOf[_u.Unliftable[T]]).asInstanceOf[UnliftListElementwise[T]]
    // def UnliftListOfListsElementwise[T](unliftable: U#Unliftable[T]): UnliftListOfListsElementwise[T] = _build.UnliftListElementwise(unliftable.asInstanceOf[_u.Unliftable[T]]).asInstanceOf[UnliftListOfListsElementwise[T]]
    def freshTermName(prefix: String): U#TermName = _build.freshTermName(prefix).asInstanceOf[U#TermName]
    def freshTypeName(prefix: String): U#TypeName = _build.freshTypeName(prefix).asInstanceOf[U#TypeName]
    def mkAnnotation(trees: List[U#Tree]): List[U#Tree] = _build.mkAnnotation(trees.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[U#Tree]]
    def mkAnnotation(tree: U#Tree): U#Tree = _build.mkAnnotation(tree.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
    def mkEarlyDef(defns: List[U#Tree]): List[U#Tree] = _build.mkEarlyDef(defns.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[U#Tree]]
    def mkEarlyDef(defn: U#Tree): U#Tree = _build.mkEarlyDef(defn.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
    def mkIdent(sym: U#Symbol): Ident = _build.mkIdent(sym.asInstanceOf[_u.Symbol]).asInstanceOf[Ident]
    def mkPackageStat(stats: List[U#Tree]): List[U#Tree] = _build.mkPackageStat(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[U#Tree]]
    def mkPackageStat(stat: U#Tree): U#Tree = _build.mkPackageStat(stat.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
    def mkRefTree(qual: U#Tree,sym: U#Symbol): U#Tree = _build.mkRefTree(qual.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[U#Tree]
    def mkRefineStat(stats: List[U#Tree]): List[U#Tree] = _build.mkRefineStat(stats.asInstanceOf[List[_u.Tree]]).asInstanceOf[List[U#Tree]]
    def mkRefineStat(stat: U#Tree): U#Tree = _build.mkRefineStat(stat.asInstanceOf[_u.Tree]).asInstanceOf[U#Tree]
    def mkSelect(qualifier: U#Tree,sym: U#Symbol): Select = _build.mkSelect(qualifier.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[Select]
    def mkThis(sym: U#Symbol): U#Tree = _build.mkThis(sym.asInstanceOf[_u.Symbol]).asInstanceOf[U#Tree]
    def mkTypeTree(tp: U#Type): TypeTree = _build.mkTypeTree(tp.asInstanceOf[_u.Type]).asInstanceOf[TypeTree]
    def newFreeTerm(name: String,value: => Any,flags: U#FlagSet,origin: String): U#FreeTermSymbol = _build.newFreeTerm(name, value, flags.asInstanceOf[_u.FlagSet], origin).asInstanceOf[U#FreeTermSymbol]
    def newFreeType(name: String,flags: U#FlagSet,origin: String): U#FreeTypeSymbol = _build.newFreeType(name, flags.asInstanceOf[_u.FlagSet], origin).asInstanceOf[U#FreeTypeSymbol]
    def newNestedSymbol(owner: U#Symbol,name: U#Name,pos: U#Position,flags: U#FlagSet,isClass: Boolean): U#Symbol = _build.newNestedSymbol(owner.asInstanceOf[_u.Symbol], name.asInstanceOf[_u.Name], pos.asInstanceOf[_u.Position], flags.asInstanceOf[_u.FlagSet], isClass).asInstanceOf[U#Symbol]
    def newScopeWith(elems: U#Symbol*): Scope = _build.newScopeWith(elems.toList.asInstanceOf[List[_u.Symbol]]: _*).asInstanceOf[Scope]
    def selectOverloadedMethod(owner: U#Symbol,name: String,index: Int): U#MethodSymbol = _build.selectOverloadedMethod(owner.asInstanceOf[_u.Symbol], name, index).asInstanceOf[U#MethodSymbol]
    def selectTerm(owner: U#Symbol,name: String): U#TermSymbol = _build.selectTerm(owner.asInstanceOf[_u.Symbol], name).asInstanceOf[U#TermSymbol]
    def selectType(owner: U#Symbol,name: String): U#TypeSymbol = _build.selectType(owner.asInstanceOf[_u.Symbol], name).asInstanceOf[U#TypeSymbol]
    def setAnnotations[S <: U#Symbol](sym: S,annots: List[U#Annotation]): S = _build.setAnnotations(sym.asInstanceOf[_u.Symbol], annots.asInstanceOf[List[_u.Annotation]]).asInstanceOf[S]
    def setInfo[S <: U#Symbol](sym: S,tpe: U#Type): S = _build.setInfo(sym.asInstanceOf[_u.Symbol], tpe.asInstanceOf[_u.Type]).asInstanceOf[S]
    def setSymbol[T <: U#Tree](tree: T,sym: U#Symbol): T = _build.setSymbol(tree.asInstanceOf[_u.Tree], sym.asInstanceOf[_u.Symbol]).asInstanceOf[T]
    def setType[T <: U#Tree](tree: T,tpe: U#Type): T = _build.setType(tree.asInstanceOf[_u.Tree], tpe.asInstanceOf[_u.Type]).asInstanceOf[T]
    def thisPrefix(sym: U#Symbol): U#Type = _build.thisPrefix(sym.asInstanceOf[_u.Symbol]).asInstanceOf[U#Type]
    def toStats(tree: U#Tree): List[U#Tree] = _build.toStats(tree.asInstanceOf[_u.Tree]).asInstanceOf[List[U#Tree]]
  }

  trait StandardLiftableImpl extends StandardLiftableApi { self: ReificationSupportImpl =>
    import u._, definitions._, Flag._

    private object Liftable {
      def apply[T](f: T => Tree): Liftable[T] = {
        new Liftable[T] { def apply(value: T): Tree = f(value) }
      }
    }

    private def lift[T: U#Liftable](value: T): Tree            = implicitly[U#Liftable[T]].apply(value).asInstanceOf[u.Tree]
    private def selectScala(names: Name*)                      = names.tail.foldLeft(ScalaDot(names.head).asInstanceOf[u.Tree]) { Select(_, _) }
    private def callScala(names: Name*)(args: List[Tree])      = Apply(selectScala(names: _*), args)
    private def callCollection(name: Name)(args: List[Tree])   = callScala(stdnme.collection, stdnme.immutable, name)(args)
    private def liftAsLiteral[T]: Liftable[T]                  = Liftable { v => Literal(Constant(v)) }

    override implicit def liftByte[T <: Byte]: Liftable[T]       = liftAsLiteral[T]
    override implicit def liftShort[T <: Short]: Liftable[T]     = liftAsLiteral[T]
    override implicit def liftChar[T <: Char]: Liftable[T]       = liftAsLiteral[T]
    override implicit def liftInt[T <: Int]: Liftable[T]         = liftAsLiteral[T]
    override implicit def liftLong[T <: Long]: Liftable[T]       = liftAsLiteral[T]
    override implicit def liftFloat[T <: Float]: Liftable[T]     = liftAsLiteral[T]
    override implicit def liftDouble[T <: Double]: Liftable[T]   = liftAsLiteral[T]
    override implicit def liftBoolean[T <: Boolean]: Liftable[T] = liftAsLiteral[T]
    override implicit def liftUnit: Liftable[Unit]               = liftAsLiteral[Unit]
    override implicit def liftString[T <: String]: Liftable[T]   = liftAsLiteral[T]

    override implicit def liftScalaSymbol: Liftable[scala.Symbol] = Liftable { v =>
      callScala(stdnme.Symbol)(Literal(Constant(v.name)) :: Nil)
    }

    override implicit def liftTree[T <: U#Tree]: Liftable[T]              = Liftable { tree => tree.asInstanceOf[u.Tree] }
    override implicit def liftName[T <: U#Name]: Liftable[T]              = Liftable { name => Ident(name.asInstanceOf[u.Name]) }
    override implicit def liftExpr[T <: U#Expr[_]]: Liftable[T]           = Liftable { expr => expr.tree.asInstanceOf[u.Tree] }
    override implicit def liftType[T <: U#Type]: Liftable[T]              = Liftable { tpe => TypeTree(tpe.asInstanceOf[u.Type]) }
    override implicit def liftTypeTag[T <: U#WeakTypeTag[_]]: Liftable[T] = Liftable { ttag => TypeTree(ttag.tpe.asInstanceOf[u.Type]) }
    override implicit def liftConstant[T <: U#Constant]: Liftable[T]      = Liftable { const => Literal(const.asInstanceOf[u.Constant]) }

    override implicit def liftArray[T: U#Liftable]: Liftable[Array[T]]               = Liftable { arr => callScala(stdnme.Array)(arr.map(lift(_)).toList) }
    override implicit def liftVector[T: U#Liftable]: Liftable[Vector[T]]             = Liftable { vect => callCollection(stdnme.Vector)(vect.map(lift(_)).toList) }
    override implicit def liftList[T: U#Liftable]: Liftable[List[T]]                 = Liftable { lst => callCollection(stdnme.List)(lst.map(lift(_))) }
    override implicit def liftNil: Liftable[Nil.type]                                = Liftable { _ => selectScala(stdnme.collection, stdnme.immutable, stdnme.Nil) }
    override implicit def liftMap[K: U#Liftable, V: U#Liftable]: Liftable[Map[K, V]] = Liftable { m => callCollection(stdnme.Map)(m.toList.map(lift(_))) }
    override implicit def liftSet[T: U#Liftable]: Liftable[Set[T]]                   = Liftable { s => callCollection(stdnme.Set)(s.toList.map(lift(_))) }

    override implicit def liftSome[T: U#Liftable]: Liftable[Some[T]]     = Liftable { case Some(v) => callScala(stdnme.Some)(lift(v) :: Nil) }
    override implicit def liftNone: Liftable[None.type]                  = Liftable { _ => selectScala(stdnme.None) }
    override implicit def liftOption[T: U#Liftable]: Liftable[Option[T]] = Liftable {
      case some: Some[T]   => lift(some)
      case none: None.type => lift(none)
    }

    override implicit def liftLeft[L: U#Liftable, R]: Liftable[Left[L, R]]                 = Liftable { case Left(v)  => callScala(stdnme.util, stdnme.Left)(lift(v) :: Nil) }
    override implicit def liftRight[L, R: U#Liftable]: Liftable[Right[L, R]]               = Liftable { case Right(v) => callScala(stdnme.util, stdnme.Right)(lift(v) :: Nil) }
    override implicit def liftEither[L: U#Liftable, R: U#Liftable]: Liftable[Either[L, R]] = Liftable {
      case left: Left[L, R]   => lift(left)
      case right: Right[L, R] => lift(right)
    }

    override implicit def liftTuple2[T1, T2](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2]): Liftable[Tuple2[T1, T2]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple3[T1, T2, T3](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3]): Liftable[Tuple3[T1, T2, T3]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple4[T1, T2, T3, T4](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4]): Liftable[Tuple4[T1, T2, T3, T4]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple5[T1, T2, T3, T4, T5](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5]): Liftable[Tuple5[T1, T2, T3, T4, T5]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple6[T1, T2, T3, T4, T5, T6](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6]): Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7]): Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8]): Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9]): Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10]): Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11]): Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12]): Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13]): Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14]): Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15]): Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16]): Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17]): Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18]): Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19]): Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20]): Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20], liftT21: U#Liftable[T21]): Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: Nil).asInstanceOf[u.Tree]
    }
    override implicit def liftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit liftT1: U#Liftable[T1], liftT2: U#Liftable[T2], liftT3: U#Liftable[T3], liftT4: U#Liftable[T4], liftT5: U#Liftable[T5], liftT6: U#Liftable[T6], liftT7: U#Liftable[T7], liftT8: U#Liftable[T8], liftT9: U#Liftable[T9], liftT10: U#Liftable[T10], liftT11: U#Liftable[T11], liftT12: U#Liftable[T12], liftT13: U#Liftable[T13], liftT14: U#Liftable[T14], liftT15: U#Liftable[T15], liftT16: U#Liftable[T16], liftT17: U#Liftable[T17], liftT18: U#Liftable[T18], liftT19: U#Liftable[T19], liftT20: U#Liftable[T20], liftT21: U#Liftable[T21], liftT22: U#Liftable[T22]): Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: liftT22(t._22) :: Nil).asInstanceOf[u.Tree]
    }
  }

  trait StandardUnliftableImpl extends StandardUnliftableApi { self: ReificationSupportImpl =>
    import u._, definitions._, Flag._

    private object Unliftable {
      def apply[T](pf: PartialFunction[Tree, T]): Unliftable[T] = new Unliftable[T] {
        def unapply(value: Tree): Option[T] = pf.lift(value)
      }
    }

    private def unliftPrimitive[Unboxed: ClassTag, Boxed: ClassTag] = Unliftable[Unboxed] {
      case Literal(Constant(value))
           if value.getClass == implicitly[ClassTag[Boxed]].runtimeClass
           || value.getClass == implicitly[ClassTag[Unboxed]].runtimeClass =>
        value.asInstanceOf[Unboxed]
    }
    override implicit def unliftByte: Unliftable[Byte]       = unliftPrimitive[Byte, java.lang.Byte]
    override implicit def unliftShort: Unliftable[Short]     = unliftPrimitive[Short, java.lang.Short]
    override implicit def unliftChar: Unliftable[Char]       = unliftPrimitive[Char, java.lang.Character]
    override implicit def unliftInt: Unliftable[Int]         = unliftPrimitive[Int, java.lang.Integer]
    override implicit def unliftLong: Unliftable[Long]       = unliftPrimitive[Long, java.lang.Long]
    override implicit def unliftFloat: Unliftable[Float]     = unliftPrimitive[Float, java.lang.Float]
    override implicit def unliftDouble: Unliftable[Double]   = unliftPrimitive[Double, java.lang.Double]
    override implicit def unliftBoolean: Unliftable[Boolean] = unliftPrimitive[Boolean, java.lang.Boolean]
    override implicit def unliftUnit: Unliftable[Unit]       = unliftPrimitive[Unit, scala.runtime.BoxedUnit]
    override implicit def unliftString: Unliftable[String]   = Unliftable { case Literal(Constant(s: String)) => s }

    override implicit def unliftScalaSymbol: Unliftable[scala.Symbol] = Unliftable {
      case Apply(ScalaDot(stdnme.Symbol), List(Literal(Constant(name: String)))) => scala.Symbol(name)
    }

    override implicit def unliftName[T <: U#Name : ClassTag]: Unliftable[T] = Unliftable[T] { case Ident(name: T) => name; case Bind(name: T, Ident(stdnme.WILDCARD)) => name }
    override implicit def unliftType: Unliftable[U#Type]                    = Unliftable[U#Type] { case tt: TypeTree if tt.tpe != null => tt.tpe }
    override implicit def unliftConstant: Unliftable[U#Constant]            = Unliftable[U#Constant] { case Literal(const) => const }

    override implicit def unliftTuple2[T1, T2](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2]): Unliftable[Tuple2[T1, T2]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: Nil) => Tuple2(v1, v2)
    }
    override implicit def unliftTuple3[T1, T2, T3](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3]): Unliftable[Tuple3[T1, T2, T3]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: Nil) => Tuple3(v1, v2, v3)
    }
    override implicit def unliftTuple4[T1, T2, T3, T4](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4]): Unliftable[Tuple4[T1, T2, T3, T4]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: Nil) => Tuple4(v1, v2, v3, v4)
    }
    override implicit def unliftTuple5[T1, T2, T3, T4, T5](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5]): Unliftable[Tuple5[T1, T2, T3, T4, T5]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: Nil) => Tuple5(v1, v2, v3, v4, v5)
    }
    override implicit def unliftTuple6[T1, T2, T3, T4, T5, T6](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6]): Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: Nil) => Tuple6(v1, v2, v3, v4, v5, v6)
    }
    override implicit def unliftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7]): Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: Nil) => Tuple7(v1, v2, v3, v4, v5, v6, v7)
    }
    override implicit def unliftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8]): Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: Nil) => Tuple8(v1, v2, v3, v4, v5, v6, v7, v8)
    }
    override implicit def unliftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9]): Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: Nil) => Tuple9(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    }
    override implicit def unliftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10]): Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: Nil) => Tuple10(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
    }
    override implicit def unliftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11]): Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: Nil) => Tuple11(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
    }
    override implicit def unliftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12]): Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: Nil) => Tuple12(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
    }
    override implicit def unliftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13]): Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: Nil) => Tuple13(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
    }
    override implicit def unliftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14]): Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: Nil) => Tuple14(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
    }
    override implicit def unliftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15]): Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: Nil) => Tuple15(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
    }
    override implicit def unliftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16]): Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: Nil) => Tuple16(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)
    }
    override implicit def unliftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17]): Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: Nil) => Tuple17(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)
    }
    override implicit def unliftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18]): Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: unliftT18(v18) :: Nil) => Tuple18(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18)
    }
    override implicit def unliftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19]): Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: unliftT18(v18) :: unliftT19(v19) :: Nil) => Tuple19(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)
    }
    override implicit def unliftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20]): Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: unliftT18(v18) :: unliftT19(v19) :: unliftT20(v20) :: Nil) => Tuple20(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20)
    }
    override implicit def unliftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20], unliftT21: U#Unliftable[T21]): Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: unliftT18(v18) :: unliftT19(v19) :: unliftT20(v20) :: unliftT21(v21) :: Nil) => Tuple21(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21)
    }
    override implicit def unliftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit unliftT1: U#Unliftable[T1], unliftT2: U#Unliftable[T2], unliftT3: U#Unliftable[T3], unliftT4: U#Unliftable[T4], unliftT5: U#Unliftable[T5], unliftT6: U#Unliftable[T6], unliftT7: U#Unliftable[T7], unliftT8: U#Unliftable[T8], unliftT9: U#Unliftable[T9], unliftT10: U#Unliftable[T10], unliftT11: U#Unliftable[T11], unliftT12: U#Unliftable[T12], unliftT13: U#Unliftable[T13], unliftT14: U#Unliftable[T14], unliftT15: U#Unliftable[T15], unliftT16: U#Unliftable[T16], unliftT17: U#Unliftable[T17], unliftT18: U#Unliftable[T18], unliftT19: U#Unliftable[T19], unliftT20: U#Unliftable[T20], unliftT21: U#Unliftable[T21], unliftT22: U#Unliftable[T22]): Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = Unliftable {
      case SyntacticTuple(unliftT1(v1) :: unliftT2(v2) :: unliftT3(v3) :: unliftT4(v4) :: unliftT5(v5) :: unliftT6(v6) :: unliftT7(v7) :: unliftT8(v8) :: unliftT9(v9) :: unliftT10(v10) :: unliftT11(v11) :: unliftT12(v12) :: unliftT13(v13) :: unliftT14(v14) :: unliftT15(v15) :: unliftT16(v16) :: unliftT17(v17) :: unliftT18(v18) :: unliftT19(v19) :: unliftT20(v20) :: unliftT21(v21) :: unliftT22(v22) :: Nil) => Tuple22(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22)
    }
  }

  private object stdnme {
    val Array      = u.newTermName("Array")
    val collection = u.newTermName("collection")
    val immutable  = u.newTermName("immutable")
    val Left       = u.newTermName("Left")
    val List       = u.newTermName("List")
    val Map        = u.newTermName("Map")
    val None       = u.newTermName("None")
    val Nil        = u.newTermName("Nil")
    val Right      = u.newTermName("Right")
    val Set        = u.newTermName("Set")
    val Some       = u.newTermName("Some")
    val Symbol     = u.newTermName("Symbol")
    val util       = u.newTermName("util")
    val Vector     = u.newTermName("Vector")
    val WILDCARD   = u.nme.WILDCARD
  }
}