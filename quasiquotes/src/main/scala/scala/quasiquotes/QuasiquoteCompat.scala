package scala.quasiquotes

import scala.reflect.api._
import scala.reflect.internal.SymbolTable
import scala.reflect.ClassTag

object QuasiquoteCompat { def apply[U <: Universe with Singleton](u0: U): QuasiquoteCompat { val u: u0.type } = new { val u: u0.type = u0 } with QuasiquoteCompat }
trait QuasiquoteCompat {
  val u: Universe
  import u._, definitions._, Flag._

  lazy val reificationSupport: build.type = build
  lazy val build: ReificationSupportApi = new ReificationSupportImpl

  // NOTE: this trait is copy/pasted from scala/scala@bcf24ec9ba
  // however the `CompatSupportApi with StandardLiftables with StandardUnliftables` part is custom to paradise 2.10.x
  trait ReificationSupportApi extends CompatSupportApi with StandardLiftables with StandardUnliftables {
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

  trait CompatSupportApi {
    object TermName {
      def apply(s: String): TermName = newTermName(s)
      def unapply(name: TermName): Some[String] = Some(name.toString)
    }

    object TypeName {
      def apply(s: String): TypeName = newTypeName(s)
      def unapply(name: TypeName): Some[String] = Some(name.toString)
    }

    object Modifiers {
      def apply(flags: FlagSet, privateWithin: Name = TermName(""), annotations: List[Tree] = Nil): Modifiers =
        u.Modifiers(flags, privateWithin, annotations)
      def unapply(mods: Modifiers): Some[(FlagSet, Name, List[Tree])] =
        Some((mods.flags, mods.privateWithin, mods.annotations))
    }

    object EmptyValDefLike {
      def apply(): ValDef = emptyValDef
      def unapply(tree: Tree): Boolean = tree eq emptyValDef
    }
  }

  trait StandardLiftables { self: ReificationSupportApi =>
    private object Liftable {
      def apply[T](f: T => Tree): Liftable[T] = {
        new Liftable[T] { def apply(value: T): Tree = f(value) }
      }
    }

    private def lift[T: Liftable](value: T): Tree            = implicitly[Liftable[T]].apply(value)
    private def selectScala(names: Name*)                    = names.tail.foldLeft(ScalaDot(names.head)) { Select(_, _) }
    private def callScala(names: Name*)(args: List[Tree])    = Apply(selectScala(names: _*), args)
    private def callCollection(name: Name)(args: List[Tree]) = callScala(stdnme.collection, stdnme.immutable, name)(args)
    private def liftAsLiteral[T]: Liftable[T]                = Liftable { v => Literal(Constant(v)) }

    implicit def liftByte[T <: Byte]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftShort[T <: Short]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftChar[T <: Char]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftInt[T <: Int]: Liftable[T]         = liftAsLiteral[T]
    implicit def liftLong[T <: Long]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftFloat[T <: Float]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftDouble[T <: Double]: Liftable[T]   = liftAsLiteral[T]
    implicit def liftBoolean[T <: Boolean]: Liftable[T] = liftAsLiteral[T]
    implicit def liftUnit: Liftable[Unit]               = liftAsLiteral[Unit]
    implicit def liftString[T <: String]: Liftable[T]   = liftAsLiteral[T]

    implicit def liftScalaSymbol: Liftable[scala.Symbol] = Liftable { v =>
      callScala(stdnme.Symbol)(Literal(Constant(v.name)) :: Nil)
    }

    implicit def liftTree[T <: Tree]: Liftable[T]              = Liftable { identity }
    implicit def liftName[T <: Name]: Liftable[T]              = Liftable { name => Ident(name) }
    implicit def liftExpr[T <: Expr[_]]: Liftable[T]           = Liftable { expr => expr.tree }
    implicit def liftType[T <: Type]: Liftable[T]              = Liftable { tpe => TypeTree(tpe) }
    implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T] = Liftable { ttag => TypeTree(ttag.tpe) }
    implicit def liftConstant[T <: Constant]: Liftable[T]      = Liftable { const => Literal(const) }

    implicit def liftArray[T: Liftable]: Liftable[Array[T]]             = Liftable { arr => callScala(stdnme.Array)(arr.map(lift(_)).toList) }
    implicit def liftVector[T: Liftable]: Liftable[Vector[T]]           = Liftable { vect => callCollection(stdnme.Vector)(vect.map(lift(_)).toList) }
    implicit def liftList[T: Liftable]: Liftable[List[T]]               = Liftable { lst => callCollection(stdnme.List)(lst.map(lift(_))) }
    implicit def liftNil: Liftable[Nil.type]                            = Liftable { _ => selectScala(stdnme.collection, stdnme.immutable, stdnme.Nil) }
    implicit def liftMap[K: Liftable, V: Liftable]: Liftable[Map[K, V]] = Liftable { m => callCollection(stdnme.Map)(m.toList.map(lift(_))) }
    implicit def liftSet[T: Liftable]: Liftable[Set[T]]                 = Liftable { s => callCollection(stdnme.Set)(s.toList.map(lift(_))) }

    implicit def liftSome[T: Liftable]: Liftable[Some[T]]     = Liftable { case Some(v) => callScala(stdnme.Some)(lift(v) :: Nil) }
    implicit def liftNone: Liftable[None.type]                = Liftable { _ => selectScala(stdnme.None) }
    implicit def liftOption[T: Liftable]: Liftable[Option[T]] = Liftable {
      case some: Some[T]   => lift(some)
      case none: None.type => lift(none)
    }

    implicit def liftLeft[L: Liftable, R]: Liftable[Left[L, R]]               = Liftable { case Left(v)  => callScala(stdnme.util, stdnme.Left)(lift(v) :: Nil) }
    implicit def liftRight[L, R: Liftable]: Liftable[Right[L, R]]             = Liftable { case Right(v) => callScala(stdnme.util, stdnme.Right)(lift(v) :: Nil) }
    implicit def liftEither[L: Liftable, R: Liftable]: Liftable[Either[L, R]] = Liftable {
      case left: Left[L, R]   => lift(left)
      case right: Right[L, R] => lift(right)
    }

    implicit def liftTuple2[T1, T2](implicit liftT1: Liftable[T1], liftT2: Liftable[T2]): Liftable[Tuple2[T1, T2]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: Nil)
    }
    implicit def liftTuple3[T1, T2, T3](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3]): Liftable[Tuple3[T1, T2, T3]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: Nil)
    }
    implicit def liftTuple4[T1, T2, T3, T4](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4]): Liftable[Tuple4[T1, T2, T3, T4]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: Nil)
    }
    implicit def liftTuple5[T1, T2, T3, T4, T5](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5]): Liftable[Tuple5[T1, T2, T3, T4, T5]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: Nil)
    }
    implicit def liftTuple6[T1, T2, T3, T4, T5, T6](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6]): Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: Nil)
    }
    implicit def liftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7]): Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: Nil)
    }
    implicit def liftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8]): Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: Nil)
    }
    implicit def liftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9]): Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: Nil)
    }
    implicit def liftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10]): Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: Nil)
    }
    implicit def liftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11]): Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: Nil)
    }
    implicit def liftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12]): Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: Nil)
    }
    implicit def liftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13]): Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: Nil)
    }
    implicit def liftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14]): Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: Nil)
    }
    implicit def liftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15]): Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: Nil)
    }
    implicit def liftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16]): Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: Nil)
    }
    implicit def liftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17]): Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: Nil)
    }
    implicit def liftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18]): Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: Nil)
    }
    implicit def liftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19]): Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: Nil)
    }
    implicit def liftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20]): Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: Nil)
    }
    implicit def liftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20], liftT21: Liftable[T21]): Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: Nil)
    }
    implicit def liftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20], liftT21: Liftable[T21], liftT22: Liftable[T22]): Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: liftT22(t._22) :: Nil)
    }
  }

  trait StandardUnliftables { self: ReificationSupportApi =>
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
    implicit def unliftByte: Unliftable[Byte]       = unliftPrimitive[Byte, java.lang.Byte]
    implicit def unliftShort: Unliftable[Short]     = unliftPrimitive[Short, java.lang.Short]
    implicit def unliftChar: Unliftable[Char]       = unliftPrimitive[Char, java.lang.Character]
    implicit def unliftInt: Unliftable[Int]         = unliftPrimitive[Int, java.lang.Integer]
    implicit def unliftLong: Unliftable[Long]       = unliftPrimitive[Long, java.lang.Long]
    implicit def unliftFloat: Unliftable[Float]     = unliftPrimitive[Float, java.lang.Float]
    implicit def unliftDouble: Unliftable[Double]   = unliftPrimitive[Double, java.lang.Double]
    implicit def unliftBoolean: Unliftable[Boolean] = unliftPrimitive[Boolean, java.lang.Boolean]
    implicit def unliftUnit: Unliftable[Unit]       = unliftPrimitive[Unit, scala.runtime.BoxedUnit]
    implicit def unliftString: Unliftable[String]   = Unliftable { case Literal(Constant(s: String)) => s }

    implicit def unliftScalaSymbol: Unliftable[scala.Symbol] = Unliftable {
      case Apply(ScalaDot(stdnme.Symbol), List(Literal(Constant(name: String)))) => scala.Symbol(name)
    }

    implicit def unliftName[T <: Name : ClassTag]: Unliftable[T] = Unliftable[T] { case Ident(name: T) => name; case Bind(name: T, Ident(stdnme.WILDCARD)) => name }
    implicit def unliftType: Unliftable[Type]                    = Unliftable[Type] { case tt: TypeTree if tt.tpe != null => tt.tpe }
    implicit def unliftConstant: Unliftable[Constant]            = Unliftable[Constant] { case Literal(const) => const }

    implicit def unliftTuple2[T1, T2](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2]): Unliftable[Tuple2[T1, T2]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: Nil) => Tuple2(v1, v2)
    }
    implicit def unliftTuple3[T1, T2, T3](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3]): Unliftable[Tuple3[T1, T2, T3]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: Nil) => Tuple3(v1, v2, v3)
    }
    implicit def unliftTuple4[T1, T2, T3, T4](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4]): Unliftable[Tuple4[T1, T2, T3, T4]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: Nil) => Tuple4(v1, v2, v3, v4)
    }
    implicit def unliftTuple5[T1, T2, T3, T4, T5](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5]): Unliftable[Tuple5[T1, T2, T3, T4, T5]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: Nil) => Tuple5(v1, v2, v3, v4, v5)
    }
    implicit def unliftTuple6[T1, T2, T3, T4, T5, T6](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6]): Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: Nil) => Tuple6(v1, v2, v3, v4, v5, v6)
    }
    implicit def unliftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7]): Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: Nil) => Tuple7(v1, v2, v3, v4, v5, v6, v7)
    }
    implicit def unliftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8]): Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: Nil) => Tuple8(v1, v2, v3, v4, v5, v6, v7, v8)
    }
    implicit def unliftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9]): Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: Nil) => Tuple9(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    }
    implicit def unliftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10]): Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: Nil) => Tuple10(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
    }
    implicit def unliftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11]): Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: Nil) => Tuple11(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
    }
    implicit def unliftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12]): Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: Nil) => Tuple12(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
    }
    implicit def unliftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13]): Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: Nil) => Tuple13(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
    }
    implicit def unliftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14]): Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: Nil) => Tuple14(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
    }
    implicit def unliftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15]): Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: Nil) => Tuple15(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
    }
    implicit def unliftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16]): Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: Nil) => Tuple16(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)
    }
    implicit def unliftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17]): Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: Nil) => Tuple17(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)
    }
    implicit def unliftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18]): Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: UnliftT18(v18) :: Nil) => Tuple18(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18)
    }
    implicit def unliftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19]): Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: UnliftT18(v18) :: UnliftT19(v19) :: Nil) => Tuple19(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)
    }
    implicit def unliftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20]): Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: UnliftT18(v18) :: UnliftT19(v19) :: UnliftT20(v20) :: Nil) => Tuple20(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20)
    }
    implicit def unliftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20], UnliftT21: Unliftable[T21]): Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: UnliftT18(v18) :: UnliftT19(v19) :: UnliftT20(v20) :: UnliftT21(v21) :: Nil) => Tuple21(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21)
    }
    implicit def unliftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20], UnliftT21: Unliftable[T21], UnliftT22: Unliftable[T22]): Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = Unliftable {
      case SyntacticTuple(UnliftT1(v1) :: UnliftT2(v2) :: UnliftT3(v3) :: UnliftT4(v4) :: UnliftT5(v5) :: UnliftT6(v6) :: UnliftT7(v7) :: UnliftT8(v8) :: UnliftT9(v9) :: UnliftT10(v10) :: UnliftT11(v11) :: UnliftT12(v12) :: UnliftT13(v13) :: UnliftT14(v14) :: UnliftT15(v15) :: UnliftT16(v16) :: UnliftT17(v17) :: UnliftT18(v18) :: UnliftT19(v19) :: UnliftT20(v20) :: UnliftT21(v21) :: UnliftT22(v22) :: Nil) => Tuple22(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22)
    }
  }

  private object stdnme {
    val Array      = newTermName("Array")
    val collection = newTermName("collection")
    val immutable  = newTermName("immutable")
    val Left       = newTermName("Left")
    val List       = newTermName("List")
    val Map        = newTermName("Map")
    val None       = newTermName("None")
    val Nil        = newTermName("Nil")
    val Right      = newTermName("Right")
    val Set        = newTermName("Set")
    val Some       = newTermName("Some")
    val Symbol     = newTermName("Symbol")
    val util       = newTermName("util")
    val Vector     = newTermName("Vector")
    val WILDCARD   = nme.WILDCARD
  }

  // TODO: making these fields private will crash at runtime with IllegalAccessError
  val _u: SymbolTable = u.asInstanceOf[SymbolTable]
  val _build = new { val global: _u.type = _u } with ReificationSupport

  class ReificationSupportImpl extends ReificationSupportApi with CompatSupportApi {
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