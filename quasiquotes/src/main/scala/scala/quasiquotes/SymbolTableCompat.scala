package scala.quasiquotes

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.internal.SymbolTable
import scala.reflect.NameTransformer
import scala.reflect.ClassTag
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.{Position, OffsetPosition, RangePosition, TransparentPosition}
import scala.reflect.internal.Flags._

trait SymbolTableCompat { self =>
  val global: SymbolTable

  lazy val gen = new { val global: self.global.type = self.global } with TreeGen
  lazy val treeInfo: TreeInfo { val global: self.global.type } = new { val global: self.global.type = self.global } with TreeInfo
  lazy val build: ReificationSupport { val global: self.global.type } = new { val global: self.global.type = self.global } with ReificationSupport

  object symbolTable {
    import global.{nameToNameOps => _, definitions => _, nme => _, tpnme => _, lowerTermNames => _, _}
    import definitions._

    type Name = global.Name

    object TermName {
      def apply(s: String): TermName = newTermName(s)
      def unapply(name: TermName): Some[String] = Some(name.toString)
    }

    object TypeName {
      def apply(s: String): TypeName = newTypeName(s)
      def unapply(name: TypeName): Some[String] = Some(name.toString)
    }

    implicit class RichTree(tree: Tree) {
      def nonEmpty = !tree.isEmpty
      def hasExistingSymbol = (tree.symbol ne null) && (tree.symbol ne NoSymbol)
      def hasAttachment[T: ClassTag]: Boolean = tree.my_attachments.get[T].isDefined
      def my_attachments: scala.reflect.macros.Attachments { type Pos = Position } = (tree: scala.reflect.macros.Universe#AttachableApi).attachments.asInstanceOf[scala.reflect.macros.Attachments { type Pos = Position }]
      def my_updateAttachment[T: ClassTag](attachment: T): Tree = { (tree: scala.reflect.macros.Universe#AttachableApi).updateAttachment(attachment); tree }
      def my_setPos(newpos: Position): Tree = {
        val apiu = global.asInstanceOf[scala.reflect.macros.Universe]
        tree.asInstanceOf[apiu.Tree].setPos(newpos.asInstanceOf[apiu.Position])
        tree
      }
      // Belongs in TreeInfo but then I can't reach it from Printers.
      private def isReferenceToScalaMember(Id: Name) = tree match {
        case Ident(Id)                                          => true
        case Select(Ident(nme.scala_), Id)                      => true
        case Select(Select(Ident(nme.ROOTPKG), nme.scala_), Id) => true
        case _                                                  => false
      }
      /** Is the tree Predef, scala.Predef, or _root_.scala.Predef?
       */
      def isReferenceToPredef = isReferenceToScalaMember(nme.Predef)
      def isReferenceToAnyVal = isReferenceToScalaMember(tpnme.AnyVal)
    }

    implicit class RichSymbol(sym: Symbol) {
      def isDefaultGetter = sym.isTerm && (sym.name.toString contains nme.DEFAULT_GETTER_STRING)
      def setterName: TermName = sym.name.setterName
      def hasVolatileType = sym.tpe.isVolatile && !sym.my_hasAnnotation(uncheckedStableClass)
      def my_hasStableFlag = sym hasFlag STABLE
      def my_hasPackageFlag = sym hasFlag PACKAGE
      def my_isSynthetic = sym.isTerm && (sym.asTerm: scala.reflect.api.Symbols#TermSymbol).isSynthetic
      def my_isLazy = sym.isTerm && (sym.asTerm: scala.reflect.api.Symbols#TermSymbol).isLazy
      def my_hasAnnotation(cls: Symbol) = sym.annotations.exists(_.tpe.typeSymbol == cls)
      def my_isStable = sym.isTerm && (sym.asTerm: scala.reflect.api.Symbols#TermSymbol).isStable
      def my_isTrait = sym.isClass && (sym.asClass: scala.reflect.api.Symbols#ClassSymbol).isTrait
      def my_isTermMacro = (sym: scala.reflect.api.Symbols#Symbol).isMacro
      def my_isVariable = sym.isTerm && (sym hasFlag MUTABLE) && !sym.isMethod
    }

    implicit class RichType(tpe: Type) {
      def my_hasAnnotation(cls: Symbol) = tpe match {
        case AnnotatedType(anns, _, _) => anns.exists(_.tpe.typeSymbol == cls)
        case _ => false
      }
      def my_isImplicit: Boolean = tpe match {
        case MethodType(p :: _, _) => p.isTerm && (p.asTerm: scala.reflect.api.Symbols#TermSymbol).isImplicit
        case PolyType(_, restpe) => restpe.my_isImplicit
        case _ => false
      }
    }

    implicit class RichMirror(rb: RootsBase) {
      final def getPackageObjectWithMember(pre: Type, sym: Symbol): Symbol = {
        // The owner of a symbol which requires package qualification may be the
        // package object iself, but it also could be any superclass of the package
        // object.  In the latter case, we must go through the qualifier's info
        // to obtain the right symbol.
        if (sym.owner.isModuleClass) sym.owner.sourceModule // fast path, if the member is owned by a module class, that must be linked to the package object
        else pre member nme.PACKAGE                         // otherwise we have to findMember
      }
    }

    implicit class RichModifiers(mods: Modifiers) {
      def my_& (flag: Long): Modifiers = {
        val flags1 = mods.flags & flag
        if (flags1 == mods.flags) mods
        else Modifiers(flags1, mods.privateWithin, mods.annotations) // setPositions mods.positions
      }
      def my_&~ (flag: Long): Modifiers = {
        val flags1 = mods.flags & (~flag)
        if (flags1 == mods.flags) mods
        else Modifiers(flags1, mods.privateWithin, mods.annotations) // setPositions mods.positions
      }
      def my_| (flag: Long): Modifiers = {
        val flags1 = mods.flags | flag
        if (flags1 == mods.flags) mods
        else Modifiers(flags1, mods.privateWithin, mods.annotations) // setPositions mods.positions
      }
      def my_withAnnotations(annots: List[Tree]) = {
        if (annots.isEmpty) mods
        else Modifiers(mods.flags, mods.privateWithin, mods.annotations ::: annots) // setPositions mods.positions
      }
      def isLocalToThis = mods hasFlag LOCAL
      def my_isCase = mods hasFlag CASE
      def my_hasAccessorFlag = mods hasFlag ACCESSOR
      def my_hasStableFlag = mods hasFlag STABLE
      def my_isImplicit = mods hasFlag IMPLICIT
      def my_isDeferred = mods hasFlag DEFERRED
      def my_isSynthetic = mods hasFlag SYNTHETIC
      def my_isLazy = mods hasFlag LAZY
      def my_isMutable = mods hasFlag MUTABLE
      def my_isTrait = mods hasFlag TRAIT
    }

    def copyValDef(tree: Tree)(
      mods: Modifiers = null,
      name: Name      = null,
      tpt: Tree       = null,
      rhs: Tree       = null
    ): ValDef = tree match {
      case ValDef(mods0, name0, tpt0, rhs0) =>
        treeCopy.ValDef(tree,
          if (mods eq null) mods0 else mods,
          if (name eq null) name0 else name,
          if (tpt eq null) tpt0 else tpt,
          if (rhs eq null) rhs0 else rhs
        )
      case t =>
        sys.error("Not a ValDef: " + t + "/" + t.getClass)
    }

    def deriveTemplate(templ: Tree)(applyToBody: List[Tree] => List[Tree]): Template = templ match {
      case Template(parents0, self0, body0) =>
        treeCopy.Template(templ, parents0, self0, applyToBody(body0))
      case t =>
        sys.error("Not a Template: " + t + "/" + t.getClass)
    }

    def copyTypeDef(tree: Tree)(
      mods: Modifiers        = null,
      name: Name             = null,
      tparams: List[TypeDef] = null,
      rhs: Tree              = null
    ): TypeDef = tree match {
      case TypeDef(mods0, name0, tparams0, rhs0) =>
        treeCopy.TypeDef(tree,
          if (mods eq null) mods0 else mods,
          if (name eq null) name0 else name,
          if (tparams eq null) tparams0 else tparams,
          if (rhs eq null) rhs0 else rhs
        )
      case t =>
        sys.error("Not a TypeDef: " + t + "/" + t.getClass)
    }

    def copyModuleDef(tree: Tree)(
      mods: Modifiers        = null,
      name: Name             = null,
      impl: Template         = null
    ): ModuleDef = tree match {
      case ModuleDef(mods0, name0, impl0) =>
        treeCopy.ModuleDef(tree,
          if (mods eq null) mods0 else mods,
          if (name eq null) name0 else name,
          if (impl eq null) impl0 else impl
        )
      case t =>
        sys.error("Not a ModuleDef: " + t + "/" + t.getClass)
    }

    object RefTree {
      def apply(qualifier: Tree, name: Name): RefTree = qualifier match {
        case EmptyTree =>
          Ident(name)
        case qual if qual.isTerm =>
          Select(qual, name)
        case qual if qual.isType =>
          assert(name.isTypeName, s"qual = $qual, name = $name")
          SelectFromTypeTree(qual, name.toTypeName)
      }
      def unapply(refTree: RefTree): Option[(Tree, Name)] = Some((refTree.qualifier, refTree.name))
    }

    implicit def enrichSymbolTable(symtab: SymbolTable): symbolTable.type = symbolTable

    implicit def AnyNameOps(name: Name): NameOps[Name]          = new NameOps(name)
    implicit def TermNameOps(name: TermName): NameOps[TermName] = new NameOps(name)
    implicit def TypeNameOps(name: TypeName): NameOps[TypeName] = new NameOps(name)

    /** FIXME: This is a good example of something which is pure "value class" but cannot
     *  reap the benefits because an (unused) $outer pointer so it is not single-field.
     */
    final class NameOps[T <: Name](name: T) {
      import NameTransformer._
      def stripSuffix(suffix: String): T = if (name.toString endsWith suffix.toString) dropRight(suffix.toString.length) else name // OPT avoid creating a Name with `suffix`
      def stripSuffix(suffix: Name): T   = if (name.toString endsWith suffix.toString) dropRight(suffix.toString.length) else name
      def take(n: Int): T                = name.my_subName(0, n).asInstanceOf[T]
      def drop(n: Int): T                = name.my_subName(n, name.toString.length).asInstanceOf[T]
      def dropRight(n: Int): T           = name.my_subName(0, name.toString.length - n).asInstanceOf[T]
      def dropLocal: TermName            = newTermName(name.toString stripSuffix nme.LOCAL_SUFFIX_STRING)
      def dropSetter: TermName           = newTermName(name.toString stripSuffix nme.SETTER_SUFFIX_STRING)
      def dropModule: T                  = this stripSuffix nme.MODULE_SUFFIX_STRING
      def localName: TermName            = getterName my_append nme.LOCAL_SUFFIX_STRING toTermName
      def setterName: TermName           = getterName my_append nme.SETTER_SUFFIX_STRING toTermName
      def getterName: TermName           = dropTraitSetterSeparator.dropSetter.dropLocal

      def my_subName(start: Int, end: Int)  = {
        if (name.isTermName) newTermName(name.toString.substring(start, end))
        else newTypeName(name.toString.substring(start, end))
      }

      def my_append(suffix: String) = {
        if (name.isTermName) newTermName(name.toString + suffix)
        else newTypeName(name.toString + suffix)
      }

      private def dropTraitSetterSeparator: TermName =
        name.toString indexOf nme.TRAIT_SETTER_SEPARATOR_STRING match {
          case -1  => name.toTermName
          case idx => newTermName(name.toString drop idx drop nme.TRAIT_SETTER_SEPARATOR_STRING.toString.length)
        }
    }

    def currentFreshNameCreator = globalFreshNameCreator

    // create fresh term/type name using implicit fresh name creator
    def freshTermName(prefix: String = nme.FRESH_TERM_NAME_PREFIX)(implicit creator: FreshNameCreator): TermName = newTermName(creator.newName(prefix))
    def freshTypeName(prefix: String)(implicit creator: FreshNameCreator): TypeName = newTypeName(creator.newName(prefix))

    // Extractor that matches names which were generated by some
    // FreshNameCreator with known prefix. Extracts user-specified
    // prefix that was used as a parameter to newName by stripping
    // global creator prefix and unique number in the end of the name.
    class FreshNameExtractor(creatorPrefix: String = "") {
      // quote prefix so that it can be used with replaceFirst
      // which expects regExp rather than simple string
      val quotedCreatorPrefix = java.util.regex.Pattern.quote(creatorPrefix)

      def unapply(name: Name): Option[String] = {
        val sname = name.toString
        // name should start with creatorPrefix and end with number
        if (!sname.startsWith(creatorPrefix) || !sname.matches("^.*\\d*$")) None
        else Some(NameTransformer.decode(sname.replaceFirst(quotedCreatorPrefix, "").replaceAll("\\d*$", "")))
      }
    }

    def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
      new OffsetPosition(source, point)

    implicit class RichPosition(position: Position.type) {
      private def validate[T <: Position](pos: T): T = {
        if (pos.isRange) assert(pos.start <= pos.end, s"bad position: ${pos.show}")
        pos
      }
      def offset(source: SourceFile, point: Int): Position                            = validate(new OffsetPosition(source, point))
      def range(source: SourceFile, start: Int, point: Int, end: Int): Position       = validate(new RangePosition(source, start, point, end))
      def transparent(source: SourceFile, start: Int, point: Int, end: Int): Position = validate(new TransparentPosition(source, start, point, end))
    }

    def duplicateAndKeepPositions(tree: Tree) = {
      // global.duplicator won't work, because it focuses range positions
      val duplicator = new Transformer { override val treeCopy = newStrictTreeCopier }
      duplicator.transform(tree)
    }
  }

  object definitions {
    import global._
    lazy val AbstractFunctionClass = 0.to(22).map(i => rootMirror.staticClass("scala.AbstractFunction" + i)).toArray
    lazy val AbstractPartialFunctionClass = rootMirror.staticClass("scala.runtime.AbstractPartialFunction")
    lazy val Any_asInstanceOf = typeOf[Any].decl(newTermName("asInstanceOf"))
    lazy val Any_isInstanceOf = typeOf[Any].decl(newTermName("isInstanceOf"))
    def isByNameParamType(tp: Type) = tp.typeSymbol == global.definitions.ByNameParamClass
    def isCastSymbol(sym: Symbol) = sym == Any_asInstanceOf || sym == Object_asInstanceOf
    def isScalaRepeatedParamType(tp: Type) = tp.typeSymbol == global.definitions.RepeatedParamClass
    def isJavaRepeatedParamType(tp: Type) = tp.typeSymbol == global.definitions.JavaRepeatedParamClass
    def isRepeatedParamType(tp: Type) = isScalaRepeatedParamType(tp) || isJavaRepeatedParamType(tp)
    def isVarArgsList(params: Seq[Symbol]) = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    lazy val FunctionClass = global.definitions.FunctionClass
    lazy val MaxFunctionArity = 22
    lazy val MaxTupleArity = 22
    lazy val NilModule = global.definitions.NilModule
    lazy val NothingClass = global.definitions.NothingClass
    lazy val NothingTpe = global.definitions.NothingTpe
    lazy val Object_asInstanceOf = typeOf[Object].member(newTermName("asInstanceOf"))
    lazy val Object_isInstanceOf = typeOf[Object].member(newTermName("isInstanceOf"))
    lazy val PartialFunctionClass = rootMirror.staticClass("scala.PartialFunction")
    lazy val Predef_??? = typeOf[Predef.type].decl(newTermName("???").encodedName)
    lazy val ReflectRuntimeUniverse = typeOf[scala.reflect.runtime.`package`.type].decl(newTermName("universe"))
    lazy val ScalaPackage = global.definitions.ScalaPackage
    lazy val SeqModule = typeOf[scala.collection.Seq.type].typeSymbol.sourceModule
    lazy val SerializableClass = typeOf[scala.Serializable].typeSymbol
    lazy val SwitchClass = typeOf[scala.annotation.switch].typeSymbol
    lazy val ThrowableClass = typeOf[java.lang.Throwable].typeSymbol
    lazy val TupleClass = global.definitions.TupleClass
    lazy val uncheckedStableClass = typeOf[scala.annotation.unchecked.uncheckedStable].typeSymbol
    lazy val Boolean_and = BooleanClass.info.decl(newTermName("&&").encodedName)
    lazy val Boolean_or = BooleanClass.info.decl(newTermName("||").encodedName)
    lazy val BooleanClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).BooleanClass.asInstanceOf[Symbol]
    lazy val ByteClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).ByteClass.asInstanceOf[Symbol]
    lazy val CharClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).CharClass.asInstanceOf[Symbol]
    lazy val DoubleClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).DoubleClass.asInstanceOf[Symbol]
    lazy val FloatClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).FloatClass.asInstanceOf[Symbol]
    lazy val IntClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).IntClass.asInstanceOf[Symbol]
    lazy val LongClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).LongClass.asInstanceOf[Symbol]
    lazy val ShortClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).ShortClass.asInstanceOf[Symbol]
    lazy val UnitClass = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).UnitClass.asInstanceOf[Symbol]
    lazy val UnitTpe = (global.definitions: scala.reflect.api.StandardDefinitions#DefinitionsApi).UnitTpe.asInstanceOf[Type]
  }

  trait CommonNames {
    import global._
    type NameType >: Null <: Name
    protected val stringToTermName = null
    protected val stringToTypeName = null
    protected implicit def createNameType(name: String): NameType
    lazy val ANON_CLASS_NAME: NameType = "$anon"
    lazy val ANON_FUN_NAME: NameType = "$anonfun"
    lazy val AnyRef: NameType = "AnyRef"
    lazy val MODULE_SUFFIX_STRING: NameType = scala.reflect.NameTransformer.MODULE_SUFFIX_STRING
    lazy val Unit: NameType = "Unit"
  }

  trait KeywordNames {
    import global._
    lazy val IFkw: TermName = newTermName("if")
    lazy val LARROWkw: TermName = newTermName("<-")
    lazy val PACKAGEkw: TermName = newTermName("package")
    lazy val YIELDkw: TermName = newTermName("yield")
  }

  object nme extends CommonNames with KeywordNames {
    import global.{nme => _, tpnme => _, _}
    type NameType = TermName
    protected implicit def createNameType(name: String): TermName = newTermName(name)
    lazy val annotation: NameType = "annotation"
    lazy val apply: NameType = "apply"
    lazy val applyDynamic: NameType = "applyDynamic"
    lazy val applyDynamicNamed: NameType = "applyDynamicNamed"
    lazy val applyOrElse: NameType = "applyOrElse"
    lazy val CHECK_IF_REFUTABLE_STRING: String = "check$ifrefutable$"
    lazy val DEFAULT_CASE: NameType = "defaultCase$"
    lazy val DEFAULT_GETTER_STRING: NameType = "$default$"
    lazy val false_ : NameType = "false"
    lazy val filter: NameType = "filter"
    lazy val flatMap: NameType = "flatMap"
    lazy val foreach: NameType = "foreach"
    def isConstructorName(name: Name) = name == CONSTRUCTOR || name == MIXIN_CONSTRUCTOR
    lazy val isDefinedAt: NameType = "isDefinedAt"
    def isLocalName(name: Name) = name.toString endsWith LOCAL_SUFFIX_STRING
    def isSetterName(name: Name) = name.toString endsWith SETTER_SUFFIX.toString
    def isVariableName(name: Name): Boolean = {
      val first = name.toString.charAt(0)
      (    ((first.isLower && first.isLetter) || first == '_')
        && (name != nme.false_)
        && (name != nme.true_)
        && (name != nme.null_)
      )
    }
    lazy val map: NameType = "map"
    lazy val MIXIN_CONSTRUCTOR: NameType = "$init$"
    lazy val Predef: NameType = "Predef"
    object raw {
      lazy val DOLLAR: NameType = "$"
    }
    lazy val REIFY_FREE_VALUE_SUFFIX: NameType = "$value"
    lazy val scala_ : NameType = "scala"
    def segments(name: String, assumeTerm: Boolean): List[Name] = {
      def mkName(str: String, term: Boolean): Name =
        if (term) newTermName(str) else newTypeName(str)

      name.indexWhere(ch => ch == '.' || ch == '#') match {
        // it's the last segment: the parameter tells us whether type or term
        case -1     => if (name == "") scala.Nil else scala.List(mkName(name, assumeTerm))
        // otherwise, we can tell based on whether '#' or '.' is the following char.
        case idx    =>
          val (simple, div, rest) = (name take idx, name charAt idx, name drop idx + 1)
          mkName(simple, div == '.') :: segments(rest, assumeTerm)
      }
    }
    lazy val selectDynamic: NameType = "selectDynamic"
    lazy val SELECTOR_DUMMY: NameType = "<unapply-selector>"
    lazy val TRAIT_SETTER_SEPARATOR_STRING: NameType = "$_setter_$"
    lazy val true_ : NameType = "true"
    lazy val unapply: NameType = "unapply"
    lazy val update: NameType = "update"
    lazy val updateDynamic: NameType = "updateDynamic"
    lazy val withFilter: NameType = "withFilter"
    lazy val FRESH_TERM_NAME_PREFIX = "x$"
    lazy val MINGT = newTermName("->").encodedName
    lazy val null_ : NameType = "null"
    lazy val QUASIQUOTE_PAT_DEF = newTermName("$quasiquote$pat$def$")
    lazy val SETTER_SUFFIX_STRING = "_$eq"
    lazy val SETTER_SUFFIX: NameType = newTermName("_=").encodedName
    lazy val WILDCARD: NameType = global.nme.WILDCARD
    lazy val EMPTY: NameType = global.nme.EMPTY
    lazy val ERROR: NameType = global.nme.ERROR
    lazy val PACKAGE: NameType = global.nme.PACKAGE
    lazy val CONSTRUCTOR: NameType = global.nme.CONSTRUCTOR
    lazy val ROOTPKG: NameType = global.nme.ROOTPKG
    lazy val LOCAL_SUFFIX_STRING: String = global.nme.LOCAL_SUFFIX_STRING
  }

  object tpnme extends CommonNames {
    import global._
    type NameType = TypeName
    protected implicit def createNameType(name: String): TypeName = newTypeName(name)
    lazy val AnyVal: NameType = "AnyVal"
    lazy val BYNAME_PARAM_CLASS_NAME: NameType = "<byname>"
    lazy val JAVA_REPEATED_PARAM_CLASS_NAME: NameType = "<repeated...>"
    lazy val Product: NameType = "Product"
    lazy val REPEATED_PARAM_CLASS_NAME: NameType = "<repeated>"
    lazy val Serializable: NameType = "Serializable"
    lazy val unchecked: NameType = "unchecked"
    lazy val WILDCARD: NameType = global.tpnme.WILDCARD
    lazy val EMPTY: NameType = global.tpnme.EMPTY
    lazy val ERROR: NameType = global.tpnme.ERROR
    lazy val PACKAGE: NameType = global.tpnme.PACKAGE
    lazy val WILDCARD_STAR: NameType = global.tpnme.WILDCARD_STAR
  }
}
