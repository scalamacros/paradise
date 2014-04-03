package org.scalamacros.paradise
package quasiquotes

import java.lang.UnsupportedOperationException
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.internal.Flags._

trait Reifiers { self: Quasiquotes =>
  import global.{build => _, gen => _, _}
  import compat.build.{nme => _, tpnme => _, _}
  import compat.treeInfo.{build => _, gen => _, nme => _, tpnme => _, _}
  import compat.symbolTable.RefTree
  import global.definitions._
  import paradiseDefinitions._
  import Rank._
  import universeTypes._

  abstract class Reifier(val isReifyingExpressions: Boolean) extends {
    val global: self.global.type = self.global
    val universe = self.universe
    val reifee = EmptyTree
    val mirror = EmptyTree
    val concrete = false
  } with ReflectReifier {
    lazy val typer = throw new UnsupportedOperationException

    def isReifyingPatterns: Boolean = !isReifyingExpressions
    def action = if (isReifyingExpressions) "unquote" else "extract"
    def holesHaveTypes = isReifyingExpressions

    /** Map that stores freshly generated names linked to the corresponding names in the reified tree.
     *  This information is used to reify names created by calls to freshTermName and freshTypeName.
     */
    val nameMap = collection.mutable.HashMap.empty[Name, Set[TermName]].withDefault { _ => Set() }

    /** Wraps expressions into:
     *    a block which starts with a sequence of vals that correspond
     *    to fresh names that has to be created at evaluation of the quasiquote
     *    and ends with reified tree:
     *
     *      {
     *        val name$1: universe.TermName = universe.build.freshTermName(prefix1)
     *        ...
     *        val name$N: universe.TermName = universe.build.freshTermName(prefixN)
     *        tree
     *      }
     *
     *  Wraps patterns into:
     *    a call into anonymous class' unapply method required by unapply macro expansion:
     *
     *      new {
     *        def unapply(tree) = tree match {
     *          case pattern if guard => Some(result)
     *          case _ => None
     *        }
     *      }.unapply(<unapply-selector>)
     *
     *    where pattern corresponds to reified tree and guard represents conjunction of equalities
     *    which check that pairs of names in nameMap.values are equal between each other.
     *  NOTE: would love to do something like that in Scala 2.10, but name-based pattern matching is not supported
     *  and introduceTopLevel-like shenanigans usually lead to big troubles with SBT
     */
    def wrap(tree: Tree) =
      if (isReifyingExpressions) {
        val freshdefs = nameMap.iterator.map {
          case (origname, names) =>
            assert(names.size == 1)
            val FreshName(prefix) = origname
            val nameTypeName = if (origname.isTermName) tpnme.TermName else tpnme.TypeName
            val freshName = if (origname.isTermName) nme.freshTermName else nme.freshTypeName
            ValDef(NoMods, names.head, Select(u, nameTypeName), mirrorCompatCall(freshName, Literal(Constant(prefix))))
        }.toList
        // q"..$freshdefs; $tree"
        SyntacticBlock(freshdefs :+ tree)
      } else {
        def unsupportedIn210() = c.abort(c.enclosingPosition, "this quasiquote pattern is only supported in Scala 2.11")
        if (unlifters.preamble.nonEmpty) unsupportedIn210()
        if (nameMap.exists(_._2.size > 1)) unsupportedIn210()
        // http://i.imgur.com/xVyoSl.jpg
        object placeholderMapper extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Bind(name, Ident(nme.WILDCARD)) if holeMap.contains(name) =>
              val UnapplyHole(_, Bind(_, originalUnapplyArg)) = holeMap(name)
              originalUnapplyArg
            case _ =>
              super.transform(tree)
          }
        }
        placeholderMapper.transform(tree)
      }

    def reifyFillingHoles(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      holeMap.unused.foreach { hole =>
        c.abort(holeMap(hole).pos, s"Don't know how to $action here")
      }
      wrap(reified)
    }

    override def reifyTree(tree: Tree): Tree =
      reifyTreePlaceholder(tree) orElse
      reifyTreeSyntactically(tree)

    def reifyTreePlaceholder(tree: Tree): Tree = tree match {
      case Placeholder(hole: ApplyHole) if hole.tpe <:< treeType => hole.tree
      case Placeholder(Hole(tree, NoDot)) if isReifyingPatterns => tree
      case Placeholder(hole @ Hole(_, rank @ Dot())) => c.abort(hole.pos, s"Can't $action with $rank here")
      case TuplePlaceholder(args) => reifyTuple(args)
      // Due to greediness of syntactic applied we need to pre-emptively peek inside.
      // `rest` will always be non-empty due to the rule on top of this one.
      case SyntacticApplied(id @ Ident(paradisenme.QUASIQUOTE_TUPLE), first :: rest) =>
        mirrorCompatCall(nme.SyntacticApplied, reifyTreePlaceholder(Apply(id, first)), reify(rest))
      case TupleTypePlaceholder(args) => reifyTupleType(args)
      case FunctionTypePlaceholder(argtpes, restpe) => reifyFunctionType(argtpes, restpe)
      case CasePlaceholder(hole) => hole.tree
      case RefineStatPlaceholder(hole) => reifyRefineStat(hole)
      case EarlyDefPlaceholder(hole) => reifyEarlyDef(hole)
      case PackageStatPlaceholder(hole) => reifyPackageStat(hole)
      case ParamPlaceholder(hole) => hole.tree
      // for enumerators are checked not during splicing but during
      // desugaring of the for loop in SyntacticFor & SyntacticForYield
      case ForEnumPlaceholder(hole) => hole.tree
      case _ => EmptyTree
    }

    def reifyTreeSyntactically(tree: Tree) = tree match {
      case RefTree(qual, SymbolPlaceholder(Hole(tree, _))) if isReifyingExpressions =>
        mirrorCompatCall(nme.mkRefTree, reify(qual), tree)
      case This(SymbolPlaceholder(Hole(tree, _))) if isReifyingExpressions =>
        mirrorCall(nme.This, tree)
      case SyntacticTraitDef(mods, name, tparams, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticTraitDef, mods, name, tparams, earlyDefs, parents, selfdef, body)
      case SyntacticClassDef(mods, name, tparams, constrmods, vparamss,
                             earlyDefs, parents, selfdef, body) =>
        mirrorCompatCall(nme.SyntacticClassDef, reify(mods), reify(name), reify(tparams), reify(constrmods),
                                                reifyVparamss(vparamss), reify(earlyDefs), reify(parents),
                                                reify(selfdef), reify(body))
      case SyntacticPackageObjectDef(name, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticPackageObjectDef, name, earlyDefs, parents, selfdef, body)
      case SyntacticObjectDef(mods, name, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticObjectDef, mods, name, earlyDefs, parents, selfdef, body)
      case SyntacticNew(earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticNew, earlyDefs, parents, selfdef, body)
      case SyntacticDefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        mirrorCompatCall(nme.SyntacticDefDef, reify(mods), reify(name), reify(tparams),
                                              reifyVparamss(vparamss), reify(tpt), reify(rhs))
      case SyntacticValDef(mods, name, tpt, rhs) if tree != emptyValDef =>
        reifyCompatCall(nme.SyntacticValDef, mods, name, tpt, rhs)
      case SyntacticVarDef(mods, name, tpt, rhs) =>
        reifyCompatCall(nme.SyntacticVarDef, mods, name, tpt, rhs)
      case SyntacticValFrom(pat, rhs) =>
        reifyCompatCall(nme.SyntacticValFrom, pat, rhs)
      case SyntacticValEq(pat, rhs) =>
        reifyCompatCall(nme.SyntacticValEq, pat, rhs)
      case SyntacticFilter(cond) =>
        reifyCompatCall(nme.SyntacticFilter, cond)
      case SyntacticFor(enums, body) =>
        reifyCompatCall(nme.SyntacticFor, enums, body)
      case SyntacticForYield(enums, body) =>
        reifyCompatCall(nme.SyntacticForYield, enums, body)
      case SyntacticAssign(lhs, rhs) =>
        reifyCompatCall(nme.SyntacticAssign, lhs, rhs)
      case SyntacticApplied(fun, argss) if argss.nonEmpty =>
        reifyCompatCall(nme.SyntacticApplied, fun, argss)
      case SyntacticTypeApplied(fun, targs) if targs.nonEmpty =>
        reifyCompatCall(nme.SyntacticTypeApplied, fun, targs)
      case SyntacticAppliedType(tpt, targs) if targs.nonEmpty =>
        reifyCompatCall(nme.SyntacticAppliedType, tpt, targs)
      case SyntacticFunction(args, body) =>
        reifyCompatCall(nme.SyntacticFunction, args, body)
      case SyntacticEmptyTypeTree() =>
        reifyCompatCall(nme.SyntacticEmptyTypeTree)
      case SyntacticImport(expr, selectors) =>
        reifyCompatCall(nme.SyntacticImport, expr, selectors)
      case SyntacticPartialFunction(cases) =>
        reifyCompatCall(nme.SyntacticPartialFunction, cases)
      case SyntacticMatch(scrutinee, cases) =>
        reifyCompatCall(nme.SyntacticMatch, scrutinee, cases)
      case SyntacticTermIdent(name, isBackquoted) =>
        reifyCompatCall(nme.SyntacticTermIdent, name, isBackquoted)
      case SyntacticTypeIdent(name) =>
        reifyCompatCall(nme.SyntacticTypeIdent, name)
      case SyntacticCompoundType(parents, defns) =>
        reifyCompatCall(nme.SyntacticCompoundType, parents, defns)
      case SyntacticSingletonType(ref) =>
        reifyCompatCall(nme.SyntacticSingletonType, ref)
      case SyntacticTypeProjection(qual, name) =>
        reifyCompatCall(nme.SyntacticTypeProjection, qual, name)
      case SyntacticAnnotatedType(tpt, annot) =>
        reifyCompatCall(nme.SyntacticAnnotatedType, tpt, annot)
      case SyntacticExistentialType(tpt, where) =>
        reifyCompatCall(nme.SyntacticExistentialType, tpt, where)
       case Q(tree) if fillListHole.isDefinedAt(tree) =>
        mirrorCompatCall(nme.SyntacticBlock, fillListHole(tree))
      case Q(other) =>
        reifyTree(other)
      // Syntactic block always matches so we have to be careful
      // not to cause infinite recursion.
      case block @ SyntacticBlock(stats) if block.isInstanceOf[Block] =>
        reifyCompatCall(nme.SyntacticBlock, stats)
      case SyntheticUnit() =>
        reifyCompatCall(nme.SyntacticBlock, Nil)
      case Try(block, catches, finalizer) =>
        reifyCompatCall(nme.SyntacticTry, block, catches, finalizer)
      case CaseDef(pat, guard, body) if fillListHole.isDefinedAt(body) =>
        mirrorCall(nme.CaseDef, reify(pat), reify(guard), mirrorCompatCall(nme.SyntacticBlock, fillListHole(body)))
      // parser emits trees with scala package symbol to ensure
      // that some names hygienically point to various scala package
      // members; we need to preserve this symbol to preserve
      // correctness of the trees produced by quasiquotes
      case Select(id @ Ident(nme.scala_), name) if id.symbol == ScalaPackage =>
        reifyCompatCall(nme.ScalaDot, name)
      case Select(qual, name) =>
        val ctor = if (name.isTypeName) nme.SyntacticSelectType else nme.SyntacticSelectTerm
        reifyCompatCall(ctor, qual, name)
      case global.EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case global.emptyValDef =>
        mirrorCompatCall(nme.EmptyValDefLike)
      case Literal(const @ Constant(_)) =>
        mirrorCall(nme.Literal, reifyProduct(const))
      case _ =>
        reifyProduct(tree)
    }

    override def reifyName(name: Name): Tree = name match {
      case Placeholder(hole: ApplyHole) =>
        if (!(hole.tpe <:< nameType)) c.abort(hole.pos, s"$nameType expected but ${hole.tpe} found")
        hole.tree
      case Placeholder(hole: UnapplyHole) => hole.treeNoUnlift
      case FreshName(prefix) if prefix != nme.QUASIQUOTE_NAME_PREFIX =>
        def fresh() = c.fresh[TermName](nme.QUASIQUOTE_NAME_PREFIX)
        def introduceName() = { val n = fresh(); nameMap(name) += n; n}
        def result(n: Name) = if (isReifyingExpressions) Ident(n) else Bind(n, Ident(nme.WILDCARD))
        if (isReifyingPatterns) result(introduceName())
        else result(nameMap.get(name).map { _.head }.getOrElse { introduceName() })
      case _ =>
        if (isReifyingExpressions) super.reifyName(name)
        else {
          val factory = if (name.isTypeName) nme.TypeName else nme.TermName
          mirrorCompatCall(factory, Literal(Constant(name.toString)))
        }
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(Hole(_, NoDot))) => reify(hole)
      case List(Placeholder(_)) => reifyCompatCall(nme.SyntacticTuple, args)
      // in a case we only have one element tuple without
      // any rank annotations this means that this is
      // just an expression wrapped in parentheses
      case List(other) => reify(other)
      case _ => reifyCompatCall(nme.SyntacticTuple, args)
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(Hole(_, NoDot))) => reify(hole)
      case List(Placeholder(_)) => reifyCompatCall(nme.SyntacticTupleType, args)
      case List(other) => reify(other)
      case _ => reifyCompatCall(nme.SyntacticTupleType, args)
    }

    def reifyFunctionType(argtpes: List[Tree], restpe: Tree) =
      reifyCompatCall(nme.SyntacticFunctionType, argtpes, restpe)

    def reifyConstructionCheck(name: TermName, hole: Hole) = hole match {
      case _: UnapplyHole => hole.tree
      case _: ApplyHole => mirrorCompatCall(name, hole.tree)
    }

    def reifyRefineStat(hole: Hole) = reifyConstructionCheck(nme.mkRefineStat, hole)

    def reifyEarlyDef(hole: Hole) = reifyConstructionCheck(nme.mkEarlyDef, hole)

    def reifyAnnotation(hole: Hole) = reifyConstructionCheck(nme.mkAnnotation, hole)

    def reifyPackageStat(hole: Hole) = reifyConstructionCheck(nme.mkPackageStat, hole)

    def reifyVparamss(vparamss: List[List[ValDef]]) = {
      val build.ImplicitParams(paramss, implparams) = vparamss
      if (implparams.isEmpty) reify(paramss)
      else reifyCompatCall(nme.ImplicitParams, paramss, implparams)
    }

    /** Splits list into a list of groups where subsequent elements are considered
     *  similar by the corresponding function.
     *
     *  Example:
     *
     *    > group(List(1, 1, 0, 0, 1, 0)) { _ == _ }
     *    List(List(1, 1), List(0, 0), List(1), List(0))
     *
     */
    def group[T](lst: List[T])(similar: (T, T) => Boolean) = lst.foldLeft[List[List[T]]](List()) {
      case (Nil, el) => List(List(el))
      case (ll :+ (last @ (lastinit :+ lastel)), el) if similar(lastel, el) => ll :+ (last :+ el)
      case (ll, el) => ll :+ List(el)
    }

    /** Reifies list filling all the valid holeMap.
     *
     *  Reification of non-trivial list is done in two steps:
     *
     *  1. split the list into groups where every placeholder is always
     *     put in a group of it's own and all subsquent non-holeMap are
     *     grouped together; element is considered to be a placeholder if it's
     *     in the domain of the fill function;
     *
     *  2. fold the groups into a sequence of lists added together with ++ using
     *     fill reification for holeMap and fallback reification for non-holeMap.
     *
     *  Example:
     *
     *    reifyHighRankList(lst) {
     *      // first we define patterns that extract high-rank holeMap (currently ..)
     *      case Placeholder(IterableType(_, _)) => tree
     *    } {
     *      // in the end we define how single elements are reified, typically with default reify call
     *      reify(_)
     *    }
     *
     *  Sample execution of previous concrete list reifier:
     *
     *    > val lst = List(foo, bar, qq$f3948f9s$1)
     *    > reifyHighRankList(lst) { ... } { ... }
     *    q"List($foo, $bar) ++ ${holeMap(qq$f3948f9s$1).tree}"
     */
    def reifyHighRankList(xs: List[Any])(fill: PartialFunction[Any, Tree])(fallback: Any => Tree): Tree

    val fillListHole: PartialFunction[Any, Tree] = {
      case Placeholder(Hole(tree, DotDot)) => tree
      case CasePlaceholder(Hole(tree, DotDot)) => tree
      case RefineStatPlaceholder(h @ Hole(_, DotDot)) => reifyRefineStat(h)
      case EarlyDefPlaceholder(h @ Hole(_, DotDot)) => reifyEarlyDef(h)
      case PackageStatPlaceholder(h @ Hole(_, DotDot)) => reifyPackageStat(h)
      case ForEnumPlaceholder(Hole(tree, DotDot)) => tree
      case ParamPlaceholder(Hole(tree, DotDot)) => tree
      case SyntacticPatDef(mods, pat, tpt, rhs) =>
        reifyCompatCall(nme.SyntacticPatDef, mods, pat, tpt, rhs)
      case SyntacticValDef(mods, p @ Placeholder(h: ApplyHole), tpt, rhs) if h.tpe <:< treeType =>
        mirrorCompatCall(nme.SyntacticPatDef, reify(mods), h.tree, reify(tpt), reify(rhs))
    }

    val fillListOfListsHole: PartialFunction[Any, Tree] = {
      case List(ParamPlaceholder(Hole(tree, DotDotDot))) => tree
      case List(Placeholder(Hole(tree, DotDotDot))) => tree
    }

    /** Reifies arbitrary list filling ..$x and ...$y holeMap when they are put
     *  in the correct position. Fallbacks to regular reification for zero rank
     *  elements.
     */
    override def reifyList(xs: List[Any]): Tree = reifyHighRankList(xs)(fillListHole.orElse(fillListOfListsHole))(reify)

    def reifyAnnotList(annots: List[Tree]): Tree = reifyHighRankList(annots) {
      case AnnotPlaceholder(h @ Hole(_, DotDot)) => reifyAnnotation(h)
    } {
      case AnnotPlaceholder(h: ApplyHole) if h.tpe <:< treeType => reifyAnnotation(h)
      case AnnotPlaceholder(h: UnapplyHole) if h.rank == NoDot => reifyAnnotation(h)
      case other => reify(other)
    }

    // These are explicit flags except those that are used
    // to overload the same tree for two different concepts:
    // - MUTABLE that is used to override ValDef for vars
    // - TRAIT that is used to override ClassDef for traits
    val nonOverloadedExplicitFlags = ExplicitFlags & ~MUTABLE & ~TRAIT

    def ensureNoExplicitFlags(m: Modifiers, pos: Position) = {
      // Traits automatically have ABSTRACT flag assigned to
      // them so in that case it's not an explicit flag
      val flags = if (m.isTrait) m.flags & ~ABSTRACT else m.flags
      if ((flags & nonOverloadedExplicitFlags) != 0L)
        c.abort(pos, s"Can't $action modifiers together with flags, consider merging flags into modifiers")
    }

    override def mirrorSelect(name: String): Tree =
      Select(universe, TermName(name))

    override def mirrorCall(name: TermName, args: Tree*): Tree =
      Apply(Select(universe, name), args.toList)

    override def mirrorCall(name: String, args: Tree*): Tree =
      mirrorCall(newTermName(name), args: _*)

    override def mirrorBuildCall(name: TermName, args: Tree*): Tree =
      Apply(Select(Select(universe, nme.build), name), args.toList)

    override def mirrorBuildCall(name: String, args: Tree*): Tree =
      mirrorBuildCall(newTermName(name), args: _*)

    def reifyBuildCall(name: TermName, args: Any*) =
      mirrorBuildCall(name, args map reify: _*)

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    // ================ REIFICATION HELPERS NECESSARY FOR SCALA 2.10 ================

    def reifyFlags(flags: FlagSet) =
      if (flags != 0) reifyCompatCall(nme.FlagsRepr, flags) else mirrorSelect(nme.NoFlags)

    // NOTE: cannot add new constructors/extractors to scala.reflect.api.BuildUtils
    // because we're just a compiler plugin, not a fork of scalac
    // therefore we have to externalize the calls to new methods
    def mirrorCompatCall(name: TermName, args: Tree*): Tree =
      Apply(Select(QuasiquoteCompatBuildRef(universe), name), args.toList)

    def reifyCompatCall(name: TermName, args: Any*) =
      mirrorCompatCall(name, args map reify: _*)
  }

  class ApplyReifier extends Reifier(isReifyingExpressions = true) {
    def reifyHighRankList(xs: List[Any])(fill: PartialFunction[Any, Tree])(fallback: Any => Tree): Tree =
      if (xs.isEmpty) mkList(Nil)
      else {
        def reifyGroup(group: List[Any]): Tree = group match {
          case List(elem) if fill.isDefinedAt(elem) => fill(elem)
          case elems => mkList(elems.map(fallback))
        }
        val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
        tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => Apply(Select(tree, nme.PLUSPLUS), List(reifyGroup(lst))) }
      }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) mirrorSelect(nme.NoMods)
      else {
        val (modsPlaceholders, annots) = m.annotations.partition {
          case ModsPlaceholder(_) => true
          case _ => false
        }
        val (mods, flags) = modsPlaceholders.map {
          case ModsPlaceholder(hole: ApplyHole) => hole
        }.partition { hole =>
          if (hole.tpe <:< modsType) true
          else if (hole.tpe <:< flagsType) false
          else c.abort(hole.pos, s"$flagsType or $modsType expected but ${hole.tpe} found")
        }
        mods match {
          case hole :: Nil =>
            if (flags.nonEmpty) c.abort(flags(0).pos, "Can't unquote flags together with modifiers, consider merging flags into modifiers")
            if (annots.nonEmpty) c.abort(hole.pos, "Can't unquote modifiers together with annotations, consider merging annotations into modifiers")
            ensureNoExplicitFlags(m, hole.pos)
            hole.tree
          case _ :: hole :: Nil =>
            c.abort(hole.pos, "Can't unquote multiple modifiers, consider merging them into a single modifiers instance")
          case _ =>
            val baseFlags = reifyFlags(m.flags)
            val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, hole) => Apply(Select(flag, nme.OR), List(hole.tree)) }
            mirrorCompatCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotList(annots))
        }
      }

  }
  class UnapplyReifier extends Reifier(isReifyingExpressions = false) {
    private def collection = ScalaDot(nme.collection)
    private def collectionColonPlus = Select(collection, nme.COLONPLUS)
    private def collectionCons = Select(Select(collection, nme.immutable), nme.CONS)
    private def collectionNil = Select(Select(collection, nme.immutable), nme.Nil)
    // pq"$lhs :+ $rhs"
    private def append(lhs: Tree, rhs: Tree) = Apply(collectionColonPlus, lhs :: rhs :: Nil)
    // pq"$lhs :: $rhs"
    private def cons(lhs: Tree, rhs: Tree) = Apply(collectionCons, lhs :: rhs :: Nil)

    def reifyHighRankList(xs: List[Any])(fill: PartialFunction[Any, Tree])(fallback: Any => Tree): Tree = {
      val grouped = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
      def appended(lst: List[Any], init: Tree)  = lst.foldLeft(init)  { (l, r) => append(l, fallback(r)) }
      def prepended(lst: List[Any], init: Tree) = lst.foldRight(init) { (l, r) => cons(fallback(l), r)   }
      grouped match {
        case init :: List(hole) :: last :: Nil if fill.isDefinedAt(hole) => appended(last, prepended(init, fill(hole)))
        case init :: List(hole) :: Nil         if fill.isDefinedAt(hole) => prepended(init, fill(hole))
        case         List(hole) :: last :: Nil if fill.isDefinedAt(hole) => appended(last, fill(hole))
        case         List(hole) :: Nil         if fill.isDefinedAt(hole) => fill(hole)
        case _                                                           => prepended(xs, collectionNil)
      }
    }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) mirrorSelect(nme.NoMods)
      else {
        val mods = m.annotations.collect { case ModsPlaceholder(hole: UnapplyHole) => hole }
        mods match {
          case hole :: Nil =>
            if (m.annotations.length != 1) c.abort(hole.pos, "Can't extract modifiers together with annotations, consider extracting just modifiers")
            ensureNoExplicitFlags(m, hole.pos)
            hole.treeNoUnlift
          case _ :: hole :: _ =>
            c.abort(hole.pos, "Can't extract multiple modifiers together, consider extracting a single modifiers instance")
          case Nil =>
            mirrorCompatCall(nme.Modifiers, reifyFlags(m.flags), reify(m.privateWithin), reifyAnnotList(m.annotations))
        }
      }
  }
}
