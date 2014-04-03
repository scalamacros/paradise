package scala.quasiquotes

import scala.language.implicitConversions
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
    import global.{nameToNameOps => _, definitions => _, _}
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
      def hasAttachment[T: ClassTag]: Boolean = tree.attachments.get[T].isDefined
    }

    implicit class RichSymbol(sym: Symbol) {
      def isDefaultGetter = sym.isTerm && (sym.name containsName nme.DEFAULT_GETTER_STRING)
      def setterName: TermName = sym.name.setterName
      def hasVolatileType = sym.tpe.isVolatile && !sym.hasAnnotation(uncheckedStableClass)
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

    implicit def nmeCompat(nme: global.nme.type): compatnme.type = compatnme
    object compatnme {
      val FRESH_TERM_NAME_PREFIX = "x$"
      val MINGT                  = TermName(NameTransformer.encode("->"))
      val QUASIQUOTE_PAT_DEF     = TermName("$quasiquote$pat$def$")
      val SETTER_SUFFIX_STRING   = "_$eq"
    }

    implicit def tpnmeCompat(nme: global.tpnme.type): compattpnme.type = compattpnme
    object compattpnme {
      final val unchecked = TypeName("unchecked")
    }

    implicit def AnyNameOps(name: Name): NameOps[Name]          = new NameOps(name)
    implicit def TermNameOps(name: TermName): NameOps[TermName] = new NameOps(name)
    implicit def TypeNameOps(name: TypeName): NameOps[TypeName] = new NameOps(name)

    /** FIXME: This is a good example of something which is pure "value class" but cannot
     *  reap the benefits because an (unused) $outer pointer so it is not single-field.
     */
    final class NameOps[T <: Name](name: T) {
      import NameTransformer._
      def stripSuffix(suffix: String): T = if (name endsWith suffix) dropRight(suffix.length) else name // OPT avoid creating a Name with `suffix`
      def stripSuffix(suffix: Name): T   = if (name endsWith suffix) dropRight(suffix.length) else name
      def take(n: Int): T                = name.subName(0, n).asInstanceOf[T]
      def drop(n: Int): T                = name.subName(n, name.length).asInstanceOf[T]
      def dropRight(n: Int): T           = name.subName(0, name.length - n).asInstanceOf[T]
      def dropLocal: TermName            = name.toTermName stripSuffix nme.LOCAL_SUFFIX_STRING
      def dropSetter: TermName           = name.toTermName stripSuffix nme.SETTER_SUFFIX_STRING
      def dropModule: T                  = this stripSuffix nme.MODULE_SUFFIX_STRING
      def localName: TermName            = getterName append nme.LOCAL_SUFFIX_STRING
      def setterName: TermName           = getterName append nme.SETTER_SUFFIX_STRING
      def getterName: TermName           = dropTraitSetterSeparator.dropSetter.dropLocal

      private def dropTraitSetterSeparator: TermName =
        name.toString indexOf nme.TRAIT_SETTER_SEPARATOR_STRING match {
          case -1  => name.toTermName
          case idx => name.toTermName drop idx drop nme.TRAIT_SETTER_SEPARATOR_STRING.length
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

    implicit class RichModifiers(mods: Modifiers) {
      def isLocalToThis = mods hasFlag LOCAL
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
    lazy val BooleanClass = global.definitions.BooleanClass
    lazy val ByteClass = global.definitions.ByteClass
    lazy val CharClass = global.definitions.CharClass
    lazy val DoubleClass = global.definitions.DoubleClass
    lazy val FloatClass = global.definitions.FloatClass
    lazy val IntClass = global.definitions.IntClass
    lazy val LongClass = global.definitions.LongClass
    lazy val ShortClass = global.definitions.ShortClass
    lazy val UnitClass = global.definitions.UnitClass
    lazy val UnitTpe = global.definitions.UnitTpe
  }
}
