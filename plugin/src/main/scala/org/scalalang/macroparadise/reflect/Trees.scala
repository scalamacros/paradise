package org.scalalang.macroparadise
package reflect

// NOTE: copy/pasted from master's BuildUtils until https://groups.google.com/d/msg/scala-language/C7Pm6ab1sPs/i9Dv0IWHZVYJ is fixed
trait Trees {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._
  import definitions._

  private def mkTparams(tparams: List[Tree]): List[TypeDef] =
    tparams.map {
      case td: TypeDef => copyTypeDef(td)(mods = (td.mods | PARAM) & (~DEFERRED))
      case other => throw new IllegalArgumentException(s"can't splice $other as type parameter")
    }

  private object ScalaDot {
    def apply(name: Name): Tree = gen.scalaDot(name)
    def unapply(tree: Tree): Option[Name] = tree match {
      case Select(id @ Ident(nme.scala_), name) if id.symbol == ScalaPackage => Some(name)
      case _ => None
    }
  }

  private object UnCtor {
    def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree])] = tree match {
      case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, Block(lvdefs, _)) =>
        Some((mods | Flag.TRAIT, Nil, lvdefs))
      case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, Block(lvdefs :+ _, _)) =>
        Some((mods, vparamss, lvdefs))
      case _ => None
    }
  }

  private object UnMkTemplate {
    def unapply(templ: Template): Option[(List[Tree], ValDef, Modifiers, List[List[ValDef]], List[Tree], List[Tree])] = {
      val Template(parents, selfType, tbody) = templ
      def result(ctorMods: Modifiers, vparamss: List[List[ValDef]], edefs: List[Tree], body: List[Tree]) =
        Some((parents, selfType, ctorMods, vparamss, edefs, body))
      def indexOfCtor(trees: List[Tree]) =
        trees.indexWhere { case UnCtor(_, _, _) => true ; case _ => false }

      if (tbody forall treeInfo.isInterfaceMember)
        result(NoMods | Flag.TRAIT, Nil, Nil, tbody)
      else if (indexOfCtor(tbody) == -1)
        None
      else {
        val (rawEdefs, rest) = tbody.span(treeInfo.isEarlyDef)
        val (gvdefs, etdefs) = rawEdefs.partition(treeInfo.isEarlyValDef)
        val (fieldDefs, UnCtor(ctorMods, ctorVparamss, lvdefs) :: body) = rest.splitAt(indexOfCtor(rest))
        val evdefs = gvdefs.zip(lvdefs).map {
          case (gvdef @ ValDef(_, _, tpt: TypeTree, _), ValDef(_, _, _, rhs)) =>
            copyValDef(gvdef)(tpt = tpt.original, rhs = rhs)
        }
        val edefs = evdefs ::: etdefs
        if (ctorMods.isTrait)
          result(ctorMods, Nil, edefs, body)
        else {
          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctorVparamss match {
            case Nil :: (tail @ ((head :: _) :: _)) if head.mods.isImplicit => tail
            case other => other
          }
          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss = mmap(vparamssRestoredImplicits) { vd =>
            val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }
          result(ctorMods, vparamss, edefs, body)
        }
      }
    }
  }

  object SyntacticClassDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
              constrMods: Modifiers, vparamss: List[List[ValDef]], earlyDefs: List[Tree],
              parents: List[Tree], selfType: ValDef, body: List[Tree]): ClassDef = {
      val extraFlags = PARAMACCESSOR | (if (mods.isCase) CASEACCESSOR else 0L)
      val vparamss0 = vparamss.map { _.map { vd => copyValDef(vd)(mods = (vd.mods | extraFlags) & (~DEFERRED)) } }
      val tparams0 = mkTparams(tparams)
      val parents0 = gen.mkParents(mods,
        if (mods.isCase) parents.filter {
          case ScalaDot(tpnme.Product | tpnme.Serializable | tpnme.AnyRef) => false
          case _ => true
        } else parents
      )
      val body0 = earlyDefs ::: body
      val templ = gen.mkTemplate(parents0, selfType, constrMods, vparamss0, body0)
      gen.mkClassDef(mods, name, tparams0, templ)
    }

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                     List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfType, ctorMods, vparamss, earlyDefs, body))
        if !ctorMods.isTrait && !ctorMods.hasFlag(JAVA) =>
        Some((mods, name, tparams, ctorMods, vparamss, earlyDefs, parents, selfType, body))
      case _ =>
        None
    }
  }

  object SyntacticTraitDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], earlyDefs: List[Tree],
              parents: List[Tree], selfType: ValDef, body: List[Tree]): ClassDef = {
      val mods0 = mods | TRAIT | ABSTRACT
      val templ = gen.mkTemplate(parents, selfType, Modifiers(TRAIT), Nil, earlyDefs ::: body)
      gen.mkClassDef(mods0, name, mkTparams(tparams), templ)
    }

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                     List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfType, ctorMods, vparamss, earlyDefs, body))
        if mods.isTrait =>
        Some((mods, name, tparams, earlyDefs, parents, selfType, body))
      case _ => None
    }
  }
}