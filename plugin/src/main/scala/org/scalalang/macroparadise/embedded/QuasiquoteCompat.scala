package scala.reflect
package api

// QuasiquoteCompat is sort of semi-synthetic
// therefore also keep an eye on reflect/Definitions.scala where it's materialized

// NOTE: please note that dependently-typed extractors used here won't normally work
// it all pans out because Reifiers.mirrorCompatCall applies some hackery to persuade scalac

object QuasiquoteCompat {

  // ==================== NEW APIS INTRODUCED IN UNIVERSE ====================

  object TermName { def apply(u0: Universe): TermNameExtractor { val u: u0.type } = new { val u: u0.type = u0 } with TermNameExtractor }
  trait TermNameExtractor extends Cake {
    import u._
    def apply(s: String): TermName = newTermName(s)
    def unapply(name: TermName): Some[String] = Some(name.toString)
  }

  object TypeName { def apply(u0: Universe): TypeNameExtractor { val u: u0.type } = new { val u: u0.type = u0 } with TypeNameExtractor }
  trait TypeNameExtractor extends Cake {
    import u._
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(name: TypeName): Some[String] = Some(name.toString)
  }

  object Modifiers { def apply(u0: Universe): ModifiersExtractor { val u: u0.type } = new { val u: u0.type = u0 } with ModifiersExtractor }
  trait ModifiersExtractor extends Cake {
    import u._
    def apply(flags: FlagSet, privateWithin: Name, annotations: List[Tree]): Modifiers =
      u.Modifiers(flags, privateWithin, annotations)
    def unapply(mods: Modifiers): Some[(FlagSet, Name, List[Tree])] =
      Some((mods.flags, mods.privateWithin, mods.annotations))
  }

  object EmptyValDefLike { def apply(u0: Universe): EmptyValDefLikeExtractor { val u: u0.type } = new { val u: u0.type = u0 } with EmptyValDefLikeExtractor }
  trait EmptyValDefLikeExtractor extends Cake {
    import u._
    def unapply(tree: Tree): Boolean = tree eq emptyValDef
  }

  // ==================== NEW APIS INTRODUCED IN BUILDUTILS ====================

  def Block(u: Universe)(stats: List[u.Tree]): u.Block = {
    import u._
    stats match {
      case Nil => u.Block(Nil, Literal(Constant(())))
      case elem :: Nil => u.Block(Nil, elem)
      case elems => u.Block(elems.init, elems.last)
    }
  }

  def mkAnnotationCtor(u: Universe)(tree: u.Tree, args: List[u.Tree]): u.Tree = {
    import u._
    tree match {
      case ident: Ident =>
        Apply(Select(New(ident), nme.CONSTRUCTOR: TermName), args)
      case call @ Apply(Select(New(ident: Ident), nme.CONSTRUCTOR), _) =>
        if (args.nonEmpty)
          throw new IllegalArgumentException("Can't splice annotation that already contains args with extra args, consider merging these lists together")
        call
      case _ =>
        throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation, consider passing Ident as a first argument")
    }
  }

  object FlagsAsBits { def apply(u0: Universe): FlagsAsBitsExtractor { val u: u0.type } = new { val u: u0.type = u0 } with FlagsAsBitsExtractor }
  trait FlagsAsBitsExtractor extends Cake {
    def unapply(flags: Long): Some[Long] = Some(flags)
  }

  object TypeApplied { def apply(u0: Universe): TypeAppliedExtractor { val u: u0.type } = new { val u: u0.type = u0 } with TypeAppliedExtractor }
  trait TypeAppliedExtractor extends Cake {
    import u._
    def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
      case TypeApply(fun, targs) => Some((fun, targs))
      case _ => Some((tree, Nil))
    }
  }

  object Applied { def apply(u0: Universe): AppliedExtractor { val u: u0.type } = new { val u: u0.type = u0 } with AppliedExtractor }
  trait AppliedExtractor extends Cake{
    import u._
    def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
      val Applied(fun, targs, argss) = tree
      targs match {
        case Nil => Some((fun, argss))
        case _ => Some((TypeApply(fun, targs), argss))
      }
    }
  }

  object SyntacticClassDef { def apply(u0: Universe): SyntacticClassDefExtractor { val u: u0.type } = new { val u: u0.type = u0 } with SyntacticClassDefExtractor }
  trait SyntacticClassDefExtractor extends Cake {
    import u._

    def apply(
        mods: Modifiers, name: TypeName, tparams: List[TypeDef],
        constrMods: Modifiers, vparamss: List[List[ValDef]],
        parents: List[Tree], argss: List[List[Tree]],
        selfdef: ValDef, body: List[Tree]): Tree = {
      SyntacticClassDef.apply(mods, name, tparams, constrMods, vparamss, parents, argss, selfdef, body)
    }

    def unapply(tree: Tree): Option[(
        Modifiers, TypeName, List[TypeDef],
        Modifiers, List[List[ValDef]],
        List[Tree], List[List[Tree]],
        ValDef, List[Tree])] = {
      SyntacticClassDef.unapply(tree)
    }
  }

  object TupleN { def apply(u0: Universe): TupleNExtractor { val u: u0.type } = new { val u: u0.type = u0 } with TupleNExtractor }
  trait TupleNExtractor extends Cake {
    import u._
    import definitions._

    def apply(args: List[Tree]): Tree = args match {
      case Nil      => Literal(Constant(()))
      case _        =>
        require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
        Apply(Ident(TupleClass(args.length).companionModule), args)
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case Literal(Constant(())) =>
        Some(Nil)
      case Apply(id: Ident, args)
        if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length).companionModule =>
        Some(args)
      case Apply(Select(Ident(pkg), tuple), args)
        if args.length <= MaxTupleArity && tuple == TupleClass(args.length).name && pkg == newTermName("scala") =>
        Some(args)
      case _ =>
        None
    }
  }

  object TupleTypeN { def apply(u0: Universe): TupleTypeNExtractor { val u: u0.type } = new { val u: u0.type = u0 } with TupleTypeNExtractor }
  trait TupleTypeNExtractor extends Cake {
    import u._
    import definitions._

    def apply(args: List[Tree]): Tree = args match {
      case Nil => Select(Ident(newTermName("scala")), newTypeName("Unit"))
      case _   =>
        require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
        AppliedTypeTree(Ident(TupleClass(args.length)), args)
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case Select(Ident(pkg), unit)
        if pkg == newTermName("scala") && unit == newTypeName("Unit") =>
        Some(Nil)
      case AppliedTypeTree(id: Ident, args)
        if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length) =>
        Some(args)
      case AppliedTypeTree(Select(id @ Ident(_), tuple), args)
        if args.length <= MaxTupleArity && id.symbol == ScalaPackage && tuple == TupleClass(args.length).name =>
        Some(args)
      case _ =>
        None
    }
  }

  def RefTree(u: Universe)(qual: u.Tree, sym: u.Symbol): u.Tree = {
    import u._
    val name = sym.name
    val result = qual match {
      case EmptyTree =>
        Ident(name)
      case qual if qual.isTerm =>
        Select(qual, name)
      case qual if qual.isType =>
        assert(name.isTypeName, s"qual = $qual, name = $name")
        SelectFromTypeTree(qual, name.toTypeName)
    }
    build.setSymbol(result, sym)
  }

  // ==================== TRICKY IMPLEMENTATIONS ====================

  trait Cake {
    val u: Universe
    import u._
    import Flag._

    class Applied(val tree: Tree) {
      def callee: Tree = {
        def loop(tree: Tree): Tree = tree match {
          case Apply(fn, _) => loop(fn)
          case tree         => tree
        }
        loop(tree)
      }

      def core: Tree = callee match {
        case TypeApply(fn, _)       => fn
        case AppliedTypeTree(fn, _) => fn
        case tree                   => tree
      }

      def targs: List[Tree] = callee match {
        case TypeApply(_, args)       => args
        case AppliedTypeTree(_, args) => args
        case _                        => Nil
      }

      def argss: List[List[Tree]] = {
        def loop(tree: Tree): List[List[Tree]] = tree match {
          case Apply(fn, args) => loop(fn) :+ args
          case _               => Nil
        }
        loop(tree)
      }

      override def toString = {
        val tstr = if (targs.isEmpty) "" else targs.mkString("[", ", ", "]")
        val astr = argss map (args => args.mkString("(", ", ", ")")) mkString ""
        s"$core$tstr$astr"
      }
    }

    object Applied {
      def apply(tree: Tree): Applied = new Applied(tree)

      def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
        Some((applied.core, applied.targs, applied.argss))

      def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
        unapply(new Applied(tree))
    }

    def isEarlyDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
      case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    def isEarlyValDef(tree: Tree) = tree match {
      case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    def isEarlyTypeDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
      case _ => false
    }

    /** Is tree legal as a member definition of an interface?
     */
    def isInterfaceMember(tree: Tree): Boolean = tree match {
      case EmptyTree                     => true
      case Import(_, _)                  => true
      case TypeDef(_, _, _, _)           => true
      case DefDef(mods, _, _, _, _, __)  => mods hasFlag DEFERRED
      case ValDef(mods, _, _, _)         => mods hasFlag DEFERRED
      case _ => false
    }

    def mkSuperSelect = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

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

    def ensureNonOverlapping(tree: Tree, others: List[Tree]){ ensureNonOverlapping(tree, others, true) }
    def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {} // FIXME: what about -Yrangepos

    def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): Template = {
      /* Add constructor to template */

      // create parameters for <init> as synthetic trees.
      var vparamss1 = vparamss map (_ map { vd =>
        atPos(vd.pos.focus) {
          val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
          val flags1 = (vd.mods.flags.asInstanceOf[Long] & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM).asInstanceOf[Long]).asInstanceOf[FlagSet]
          val mods = u.Modifiers(flags1 | PARAM | PARAMACCESSOR)
          // FIXME: val mods1 = mods.withAnnotations(vd.mods.annotations)
          val mods1 = mods
          ValDef(mods1, vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
        }
      })
      val (edefs, rest) = body span isEarlyDef
      val (evdefs, etdefs) = edefs partition isEarlyValDef
      val gvdefs = evdefs map {
        case vdef @ ValDef(_, _, tpt, _) =>
          copyValDef(vdef)(
          // atPos for the new tpt is necessary, since the original tpt might have no position
          // (when missing type annotation for ValDef for example), so even though setOriginal modifies the
          // position of TypeTree, it would still be NoPosition. That's what the author meant.
          // FIXME: tpt = atPos(vdef.pos.focus)(TypeTree() setOriginal tpt setPos tpt.pos.focus),
          tpt = atPos(vdef.pos.focus)(TypeTree()),
          rhs = EmptyTree
        )
      }
      // FIXME: val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods | PRESUPER) }
      val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods) }

      val constrs = {
        if (constrMods hasFlag TRAIT) {
          if (body forall isInterfaceMember) List()
          else List(
            atPos(wrappingPos(superPos, lvdefs)) (
              DefDef(NoMods, newTermName("$init$"), List(), List(List()), TypeTree(), u.Block(lvdefs, Literal(Constant(()))))))
        } else {
          // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
          if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.hasFlag(IMPLICIT))
            vparamss1 = List() :: vparamss1;
          val superRef: Tree = atPos(superPos)(mkSuperSelect)
          val superCall = (superRef /: argss) (Apply.apply)
          List(
            atPos(wrappingPos(superPos, lvdefs ::: argss.flatten)) (
              DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), u.Block(lvdefs ::: List(superCall), Literal(Constant(()))))))
        }
      }
      constrs foreach (ensureNonOverlapping(_, parents ::: gvdefs, focus=false))
      // Field definitions for the class - remove defaults.
      // FIXME: val fieldDefs = vparamss.flatten map (vd => copyValDef(vd)(mods = vd.mods &~ DEFAULTPARAM, rhs = EmptyTree))
      val fieldDefs = vparamss.flatten map (vd => copyValDef(vd)(mods = vd.mods, rhs = EmptyTree))

      u.Template(parents, self, gvdefs ::: fieldDefs ::: constrs ::: etdefs ::: rest)
    }

    object SyntacticClassDef {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]],
                parents: List[Tree], argss: List[List[Tree]],
                selfdef: ValDef, body: List[Tree]): Tree = {
        ClassDef(mods, name, tparams, Template(parents, selfdef, constrMods, vparamss, argss, body, NoPosition))
      }

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       Modifiers, List[List[ValDef]],
                                       List[Tree], List[List[Tree]],
                                       ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) if (mods.flags.asInstanceOf[Long] & scala.reflect.internal.Flags.TRAIT) != 0 =>
          Some((mods, name, tparams, NoMods, List(Nil), parents, Nil, selfdef, tbody))

        case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
          // extract generated fieldDefs and constructor
          val (defs, (ctor: DefDef) :: body) = tbody.splitAt(tbody.indexWhere {
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true
            case _ => false
          })
          val (imports, others) = defs.span(_.getClass == ImportTag.runtimeClass)
          val (earlyDefs, fieldDefs) = defs.span(isEarlyDef)
          val (fieldValDefs, fieldAccessorDefs) = fieldDefs.partition(_.getClass == ValDefTag.runtimeClass)

          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctor.vparamss match {
            case Nil :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.hasFlag(IMPLICIT) => rest
            case other => other
          }

          val (argss, superPos) = ctor.rhs match {
            case Block(_ :+ ValDef(_, _, _, _), Literal(Constant(()))) => (Nil, NoPosition)
            case Block(_ :+ (superCall @ Applied(core, _, argss)), Literal(Constant(()))) => (argss, core.pos)
            case _ => (Nil, NoPosition)
          }

          // undo flag modifications by merging flag info from constructor args and fieldValDefs
          val modsMap = fieldValDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss = vparamssRestoredImplicits.map(_.map(vd => {
            val mods1 = modsMap(vd.name)
            val flags1 = mods1.flags.asInstanceOf[Long]
            val flags2 = flags1 | (vd.mods.flags.asInstanceOf[Long] & DEFAULTPARAM.asInstanceOf[Long])
            val originalMods =
              if (flags1 == flags2) mods1
              else u.Modifiers(flags2.asInstanceOf[FlagSet], mods1.privateWithin, mods1.annotations) /* FIXME: setPositions positions */
            // val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }))

          Some((mods, name, tparams, ctor.mods, vparamss, parents, argss, selfdef, imports ::: earlyDefs ::: body))
        case _ =>
          None
      }
    }

    val MaxTupleArity = 22

    implicit class RichSymbol(sym: Symbol) {
      def companionModule = {
        val internalSym = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
        internalSym.companionModule.asInstanceOf[Symbol]
      }
    }
  }
}