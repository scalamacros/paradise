package org.scalamacros.paradise
package quasiquotes

import java.lang.UnsupportedOperationException
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.internal.Flags._

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.treeInfo._
  import global.definitions._
  import paradiseDefinitions._
  import Cardinality._
  import universeTypes._

  abstract class Reifier extends {
    val global: self.global.type = self.global
    val universe = self.universe
    val reifee = EmptyTree
    val mirror = EmptyTree
    val concrete = false
  } with ReflectReifier {
    lazy val typer = throw new UnsupportedOperationException

    def isReifyingExpressions: Boolean
    def isReifyingPatterns: Boolean = !isReifyingExpressions
    def action = if (isReifyingExpressions) "splice" else "extract"
    def holesHaveTypes = isReifyingExpressions

    def reifyFillingHoles(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      holeMap.unused.foreach { hole =>
        c.abort(holeMap(hole).tree.pos, s"Don't know how to $action here")
      }
      reified
    }

    override def reifyTree(tree: Tree): Tree =
      reifyTreePlaceholder(tree) orElse
      reifyTreeSyntactically(tree)

    def reifyTreePlaceholder(tree: Tree): Tree = tree match {
      case Placeholder(tree, TreeLocation(_), _) if isReifyingExpressions => tree
      case Placeholder(tree, _, NoDot) if isReifyingPatterns => tree
      case Placeholder(tree, _, card @ Dot()) => c.abort(tree.pos, s"Can't $action with $card here")
      case TuplePlaceholder(args) => reifyTuple(args)
      case TupleTypePlaceholder(args) => reifyTupleType(args)
      case FunctionTypePlaceholder(argtpes, restpe) => reifyFunctionType(argtpes, restpe)
      case CasePlaceholder(tree, location, _) => reifyCase(tree, location)
      case RefineStatPlaceholder(tree, _, _) => reifyRefineStat(tree)
      case EarlyDefPlaceholder(tree, _, _) => reifyEarlyDef(tree)
      case _ => EmptyTree
    }

    def reifyTreeSyntactically(tree: Tree) = tree match {
      case compat.SyntacticTraitDef(mods, name, tparams, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticTraitDef, mods, name, tparams, earlyDefs, parents, selfdef, body)
      case compat.SyntacticClassDef(mods, name, tparams, constrmods, vparamss, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticClassDef, mods, name, tparams, constrmods, vparamss,
                                               earlyDefs, parents, selfdef, body)
      case compat.SyntacticModuleDef(mods, name, earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticModuleDef, mods, name, earlyDefs, parents, selfdef, body)
      case compat.SyntacticNew(earlyDefs, parents, selfdef, body) =>
        reifyCompatCall(nme.SyntacticNew, earlyDefs, parents, selfdef, body)
      case compat.SyntacticDefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        reifyCompatCall(nme.SyntacticDefDef, mods, name, tparams, vparamss, tpt, rhs)
      case compat.SyntacticValDef(mods, name, tpt, rhs) =>
        reifyCompatCall(nme.SyntacticValDef, mods, name, tpt, rhs)
      case compat.SyntacticVarDef(mods, name, tpt, rhs) =>
        reifyCompatCall(nme.SyntacticVarDef, mods, name, tpt, rhs)
      case compat.SyntacticAssign(lhs, rhs) =>
        reifyCompatCall(nme.SyntacticAssign, lhs, rhs)
      case compat.SyntacticApplied(fun, argss) if argss.length > 1 =>
        reifyCompatCall(nme.SyntacticApplied, fun, argss)
      case compat.SyntacticApplied(fun, argss @ (_ :+ (_ :+ Placeholder(_, _, DotDotDot)))) =>
        reifyCompatCall(nme.SyntacticApplied, fun, argss)
      case compat.SyntacticTypeApplied(fun, targs) if targs.nonEmpty =>
        reifyCompatCall(nme.SyntacticTypeApplied, fun, targs)
      case compat.SyntacticFunction(args, body) =>
        reifyCompatCall(nme.SyntacticFunction, args, body)
      case Block(stats, last) =>
        reifyCompatCall(nme.SyntacticBlock, stats :+ last)
      // parser emits trees with scala package symbol to ensure
      // that some names hygienically point to various scala package
      // members; we need to preserve this symbol to preserve
      // correctness of the trees produced by quasiquotes
      case Select(id @ Ident(nme.scala_), name) if id.symbol == ScalaPackage =>
        reifyCompatCall(nme.ScalaDot, name)
      case _ =>
        superReifyTreeSyntactically(tree)
    }

    def superReifyTreeSyntactically(tree: Tree) = tree match {
      case global.EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case global.emptyValDef =>
        if (isReifyingExpressions) mirrorSelect(nme.emptyValDef)
        else mirrorCompatCall(nme.EmptyValDefLike)
      case Literal(const @ Constant(_)) =>
        mirrorCall(nme.Literal, reifyProduct(const))
      case Import(expr, selectors) =>
        mirrorCall(nme.Import, reify(expr), mkList(selectors map reifyProduct))
      case _ =>
        reifyProduct(tree)
    }

    override def reifyName(name: Name): Tree = name match {
      case Placeholder(tree, location, _) =>
        if (holesHaveTypes && !(location.tpe <:< nameType)) c.abort(tree.pos, s"$nameType expected but ${location.tpe} found")
        tree
      case _ =>
        if (isReifyingExpressions) {
          // it's good to use newTermName/newTypeName for tree construction
          // so that we don't have to pull QuasiquoteCompat in when we don't to
          super.reifyName(name)
        } else {
          val factory = if (name.isTypeName) nme.TypeName else nme.TermName
          mirrorCompatCall(factory, Literal(Constant(name.toString)))
        }
    }

    def reifyCase(tree: Tree, location: Location) = {
      if (holesHaveTypes && !(location.tpe <:< caseDefType)) c.abort(tree.pos, s"$caseDefType expected but ${location.tpe} found")
      tree
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(_, _, NoDot)) => reify(hole)
      case List(Placeholder(_, _, _)) => reifyCompatCall(nme.SyntacticTuple, args)
      // in a case we only have one element tuple without
      // any cardinality annotations this means that this is
      // just an expression wrapped in parentheses
      case List(other) => reify(other)
      case _ => reifyCompatCall(nme.SyntacticTuple, args)
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(_, _, NoDot)) => reify(hole)
      case List(Placeholder(_, _, _)) => reifyCompatCall(nme.SyntacticTupleType, args)
      case List(other) => reify(other)
      case _ => reifyCompatCall(nme.SyntacticTupleType, args)
    }

    def reifyFunctionType(argtpes: List[Tree], restpe: Tree) =
      reifyCompatCall(nme.SyntacticFunctionType, argtpes, restpe)

    def reifyRefineStat(tree: Tree) = tree

    def reifyEarlyDef(tree: Tree) = tree

    def reifyAnnotation(tree: Tree) = tree

    def reifyFlags(flags: FlagSet) =
      if (flags != 0) reifyCompatCall(nme.FlagsRepr, flags) else mirrorSelect(nme.NoFlags)

    override def reifyModifiers(m: global.Modifiers) =
      if (m == NoMods) mirrorSelect(nme.NoMods)
      else mirrorFactoryCall(nme.Modifiers, reifyFlags(m.flags), reify(m.privateWithin), reify(m.annotations))

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
     *    reifyMultiCardinalityList(lst) {
     *      // first we define patterns that extract high-cardinality holeMap (currently ..)
     *      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< iterableTreeType => tree
     *    } {
     *      // in the end we define how single elements are reified, typically with default reify call
     *      reify(_)
     *    }
     *
     *  Sample execution of previous concrete list reifier:
     *
     *    > val lst = List(foo, bar, qq$f3948f9s$1)
     *    > reifyMultiCardinalityList(lst) { ... } { ... }
     *    q"List($foo, $bar) ++ ${holeMap(qq$f3948f9s$1).tree}"
     */
    def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree

    /** Reifies arbitrary list filling ..$x and ...$y holeMap when they are put
     *  in the correct position. Fallbacks to regular reification for non-high cardinality
     *  elements.
     */
    override def reifyList(xs: List[Any]): Tree = reifyMultiCardinalityList(xs) {
      case Placeholder(tree, _, DotDot) => tree
      case CasePlaceholder(tree, _, DotDot) => tree
      case RefineStatPlaceholder(tree, _, DotDot) => reifyRefineStat(tree)
      case EarlyDefPlaceholder(tree, _, DotDot) => reifyEarlyDef(tree)
      case List(Placeholder(tree, _, DotDotDot)) => tree
    } {
      reify(_)
    }

    def reifyAnnotList(annots: List[Tree]): Tree = reifyMultiCardinalityList(annots) {
      case AnnotPlaceholder(tree, _, DotDot) => reifyAnnotation(tree)
    } {
      case AnnotPlaceholder(tree, UnknownLocation | TreeLocation(_), NoDot) => reifyAnnotation(tree)
      case other => reify(other)
    }

    // These are explicit flags except those that are used
    // to overload the same tree for two different concepts:
    // - MUTABLE that is used to override ValDef for vars
    // - TRAIT that is used to override ClassDef for traits
    val nonoverloadedExplicitFlags = ExplicitFlags & ~MUTABLE & ~TRAIT

    def ensureNoExplicitFlags(m: Modifiers, pos: Position) = {
      // Traits automatically have ABSTRACT flag assigned to
      // them so in that case it's not an explicit flag
      val flags = if (m.isTrait) m.flags & ~ABSTRACT else m.flags
      if ((flags & nonoverloadedExplicitFlags) != 0L)
        c.abort(pos, s"Can't $action modifiers together with flags, consider merging flags into modifiers")
    }

    override def mirrorSelect(name: String): Tree =
      Select(universe, newTermName(name))

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

    // NOTE: cannot add new constructors/extractors to scala.reflect.api.BuildUtils
    // because we're just a compiler plugin, not a fork of scalac
    // therefore we have to externalize the calls to new methods
    def mirrorCompatCall(name: TermName, args: Tree*): Tree =
      if (isReifyingExpressions)
        Apply(Select(Apply(Select(termPath(QuasiquoteCompatModule.fullName), nme.apply), List(universe)), name), args.toList)
      else {
        // NOTE: density of workarounds per line of code is rapidly ramping up!
        // here we force the compiler into supporting path-dependent extractors
        // by manually applying the first argument list, i.e. the one that contains the universe,
        // and then setting the type of the resulting Apply node so that the typechecker doesn't touch it later
        // if we didn't set the type, then typedApply for the first arglist would get confused and fai
        val compatModule = QuasiquoteCompatModule.moduleClass
        val compatApply = compatModule.info.decl(nme.apply)
        val apply = gen.mkAttributedRef(compatModule.tpe, compatApply)
        val compatInstance = Apply(apply, List(universe)) setType apply.tpe.resultType(List(universe.tpe))
        Apply(Select(compatInstance, name), args.toList)
      }

    def reifyCompatCall(name: TermName, args: Any*) =
      mirrorCompatCall(name, args map reify: _*)
  }

  class ApplyReifier extends Reifier {
    def isReifyingExpressions = true

    override def reifyTreeSyntactically(tree: Tree): Tree = tree match {
      case compat.RefTree(qual, SymbolPlaceholder(tree)) =>
        mirrorCompatCall(nme.RefTree, reify(qual), tree)
      case _ =>
        super.reifyTreeSyntactically(tree)
    }

    override def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree = xs match {
      case Nil => mkList(Nil)
      case _ =>
        def reifyGroup(group: List[T]): Tree = group match {
          case List(elem) if fill.isDefinedAt(elem) => fill(elem)
          case elems => mkList(elems.map(fallback))
        }
        val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
        tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => Apply(Select(tree, nme.PLUSPLUS), List(reifyGroup(lst))) }
    }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) super.reifyModifiers(m)
      else {
        val (modsPlaceholders, annots) = m.annotations.partition {
          case ModsPlaceholder(_, _, _) => true
          case _ => false
        }
        val (mods, flags) = modsPlaceholders.map {
          case ModsPlaceholder(tree, location, card) => (tree, location)
        }.partition { case (tree, location) =>
          location match {
            case ModsLocation => true
            case FlagsLocation => false
            case _ => c.abort(tree.pos, s"$flagsType or $modsType expected but ${tree.tpe} found")
          }
        }
        mods match {
          case (tree, _) :: Nil =>
            if (flags.nonEmpty) c.abort(flags(0)._1.pos, "Can't splice flags together with modifiers, consider merging flags into modifiers")
            if (annots.nonEmpty) c.abort(tree.pos, "Can't splice modifiers together with annotations, consider merging annotations into modifiers")
            ensureNoExplicitFlags(m, tree.pos)
            tree
          case _ :: (second, _) :: Nil =>
            c.abort(second.pos, "Can't splice multiple modifiers, consider merging them into a single modifiers instance")
          case _ =>
            val baseFlags = reifyFlags(m.flags)
            val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, (tree, _)) => Apply(Select(flag, nme.OR), List(tree)) }
            mirrorCompatCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotList(annots))
        }
      }

    override def reifyRefineStat(tree: Tree) = mirrorCompatCall(nme.mkRefineStat, tree)

    override def reifyEarlyDef(tree: Tree) = mirrorCompatCall(nme.mkEarlyDef, tree)

    override def reifyAnnotation(tree: Tree) = mirrorCompatCall(nme.mkAnnotation, tree)
  }

  class UnapplyReifier extends Reifier {
    def isReifyingExpressions = false

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    override def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree) = xs match {
      case init :+ last if fill.isDefinedAt(last) =>
        init.foldRight[Tree](fill(last)) { (el, rest) =>
          val cons = Select(Select(Select(Ident(nme.scala_), nme.collection), nme.immutable), nme.CONS)
          Apply(cons, List(fallback(el), rest))
        }
      case _ =>
        mkList(xs.map(fallback))
    }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) super.reifyModifiers(m)
      else {
        val mods = m.annotations.collect { case ModsPlaceholder(tree, _, _) => tree }
        mods match {
          case tree :: Nil =>
            if (m.annotations.length != 1) c.abort(tree.pos, "Can't extract modifiers together with annotations, consider extracting just modifiers")
            ensureNoExplicitFlags(m, tree.pos)
            tree
          case _ :: second :: rest =>
            c.abort(second.pos, "Can't extract multiple modifiers together, consider extracting a single modifiers instance")
          case Nil =>
            mirrorCompatCall(nme.Modifiers, reifyFlags(m.flags), reify(m.privateWithin), reifyAnnotList(m.annotations))
        }
      }
  }
}