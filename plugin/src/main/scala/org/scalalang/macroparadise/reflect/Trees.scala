package org.scalalang.macroparadise
package reflect

trait Trees {
  self: Enrichments =>

  import global._
  import scala.reflect.internal.Flags._

  // copy/pasted from BuildUtils.scala with a couple fixes applied
  // TODO: remove it once the fixes are propagated to master
  //
  // list of fixes:
  // 1) filter out the static import that comes before the primary ctor when we parse *.java files
  // 2) filter out getters and setters
  // 3) abstract trait Factory[A] extends scala.AnyRef => empty template body
  // 4) don't try to reconstruct vparamss of java classes
  object SyntacticClassDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
              constrMods: Modifiers, vparamss: List[List[ValDef]], parents: List[Tree],
              selfdef: ValDef, body: List[Tree]): Tree =
      ClassDef(mods, name, tparams, gen.mkTemplate(parents, selfdef, constrMods, vparamss, body, NoPosition))

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers,
                                     List[List[ValDef]], List[Tree], ValDef, List[Tree])] = tree match {
      case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
        val iofCtor = tbody.indexWhere { case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true; case _ => false }
        if (iofCtor != -1 && !mods.hasFlag(JAVA)) {
          // extract generated fieldDefs and constructor
          val (defs, (ctor: DefDef) :: body) = tbody.splitAt(iofCtor)
          val (imports, others) = defs.span(_.isInstanceOf[Import])
          val (earlyDefs, fieldDefs) = defs.span(treeInfo.isEarlyDef)
          val (fieldValDefs, fieldAccessorDefs) = fieldDefs.partition(_.isInstanceOf[ValDef])

          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctor.vparamss match {
            case Nil :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.isImplicit => rest
            case other => other
          }

          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldValDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss = mmap(vparamssRestoredImplicits) { vd =>
            val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }

          Some((mods, name, tparams, ctor.mods, vparamss, parents, selfdef, earlyDefs ::: body))
        } else {
          // bail out of body parsing/reconstruction
          Some((mods, name, tparams, NoMods, Nil, parents, selfdef, tbody))
        }
      case _ =>
        None
    }
  }
}
