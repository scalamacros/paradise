package org.scalalang.macroparadise
package reflect

trait Trees {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  def duplicateAndKeepPositions(tree: Tree) = {
    // global.duplicator won't work, because it focuses range positions
    val duplicator = new Transformer { override val treeCopy = newStrictTreeCopier }
    duplicator.transform(tree)
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

  object SyntacticClassDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
              constrMods: Modifiers, vparamss: List[List[ValDef]],
              parents: List[Tree], argss: List[List[Tree]],
              selfdef: ValDef, body: List[Tree], superPos: Position): Tree = {
      ClassDef(mods, name, tparams, Template(parents, selfdef, constrMods, vparamss, argss, body, superPos))
    }

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                     Modifiers, List[List[ValDef]],
                                     List[Tree], List[List[Tree]],
                                     ValDef, List[Tree], Position)] = tree match {
      case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) if (mods.flags & TRAIT) != 0 =>
        Some((mods, name, tparams, NoMods, List(Nil), parents, Nil, selfdef, tbody, NoPosition))

      case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
        // extract generated fieldDefs and constructor
        val (defs, (ctor: DefDef) :: body) = tbody.splitAt(tbody.indexWhere {
          case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true
          case _ => false
        })
        val (imports, others) = defs.span(_.isInstanceOf[Import])
        val (earlyDefs, fieldDefs) = defs.span(treeInfo.isEarlyDef)
        val (fieldValDefs, fieldAccessorDefs) = fieldDefs.partition(_.isInstanceOf[ValDef])

        // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
        val vparamssRestoredImplicits = ctor.vparamss match {
          case Nil :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.isImplicit => rest
          case other => other
        }

        val (argss, superPos) = ctor.rhs match {
          case Block(_ :+ ValDef(_, _, _, _), Literal(Constant(()))) => (Nil, NoPosition)
          case Block(_ :+ (superCall @ treeInfo.Applied(core, _, argss)), Literal(Constant(()))) => (argss, core.pos)
          case _ => (Nil, NoPosition)
        }

        // undo flag modifications by merging flag info from constructor args and fieldValDefs
        val modsMap = fieldValDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
        val vparamss = mmap(vparamssRestoredImplicits) { vd =>
          val defaultMods = Modifiers(PRIVATE | LOCAL | PARAMACCESSOR)
          val originalMods = modsMap.get(vd.name).getOrElse(defaultMods) | (vd.mods.flags & DEFAULTPARAM)
          atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
        }

        Some((mods, name, tparams, ctor.mods, vparamss, parents, argss, selfdef, imports ::: earlyDefs ::: body, superPos))
      case _ =>
        None
    }
  }
}
