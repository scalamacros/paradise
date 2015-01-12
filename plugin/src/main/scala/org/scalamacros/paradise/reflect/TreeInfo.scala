package org.scalamacros.paradise
package reflect

trait TreeInfo {
  self: Enrichments =>

  import global._
  import definitions._
  import build.{SyntacticClassDef, SyntacticTraitDef}

  implicit class ParadiseTreeInfo(treeInfo: global.treeInfo.type) {
    def primaryConstructorArity(tree: ClassDef): Int = treeInfo.firstConstructor(tree.impl.body) match {
      case DefDef(_, _, _, params :: _, _, _) => params.length
    }

    def anyConstructorHasDefault(tree: ClassDef): Boolean = tree.impl.body exists {
      case DefDef(_, nme.CONSTRUCTOR, _, paramss, _, _) => mexists(paramss)(_.mods.hasDefault)
      case _                                            => false
    }

    def isMacroAnnotation(tree: ClassDef): Boolean = {
      val clazz = tree.symbol
      def isAnnotation = clazz isNonBottomSubClass AnnotationClass
      def hasMacroTransformMethod = clazz.info.member(nme.macroTransform) != NoSymbol
      clazz != null && isAnnotation && hasMacroTransformMethod
    }

    // TODO: no immediate idea how to write this in a sane way
    def getAnnotationZippers(tree: Tree): List[AnnotationZipper] = {
      def loop[T <: Tree](tree: T, deep: Boolean): List[AnnotationZipper] = tree match {
        case SyntacticClassDef(mods, name, tparams, constrMods, vparamss, earlyDefs, parents, selfdef, body) =>
          val cdef = tree.asInstanceOf[ClassDef]
          val czippers = mods.annotations.map(ann => {
            val annottee = cdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
          if (!deep) czippers
          else {
            val tzippers = for {
              tparam <- tparams
              AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
              tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
            } yield AnnotationZipper(ann, tparam1, cdef.copy(tparams = tparams1))
            val vzippers = for {
              vparams <- vparamss
              vparam <- vparams
              AnnotationZipper(ann, vparam1: ValDef, _) <- loop(vparam, deep = false)
              vparams1 = vparams.updated(vparams.indexOf(vparam), vparam1)
              vparamss1 = vparamss.updated(vparamss.indexOf(vparams), vparams1)
            } yield AnnotationZipper(ann, vparam1, PatchedSyntacticClassDef(mods, name, tparams, constrMods, vparamss1, earlyDefs, parents, selfdef, body))
            czippers ++ tzippers ++ vzippers
          }
        case SyntacticTraitDef(mods, name, tparams, earlyDefs, parents, selfdef, body) =>
          val tdef = tree.asInstanceOf[ClassDef]
          val czippers = mods.annotations.map(ann => {
            val annottee = tdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
          if (!deep) czippers
          else {
            val tzippers = for {
              tparam <- tparams
              AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
              tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
            } yield AnnotationZipper(ann, tparam1, tdef.copy(tparams = tparams1))
            czippers ++ tzippers
          }
        case mdef @ ModuleDef(mods, _, _) =>
          mods.annotations.map(ann => {
            val annottee = mdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
        case ddef @ DefDef(mods, _, tparams, vparamss, _, _) =>
          val dzippers = mods.annotations.map(ann => {
            val annottee = ddef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
          if (!deep) dzippers
          else {
            val tzippers = for {
              tparam <- tparams
              AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
              tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
            } yield AnnotationZipper(ann, tparam1, ddef.copy(tparams = tparams1))
            val vzippers = for {
              vparams <- vparamss
              vparam <- vparams
              AnnotationZipper(ann, vparam1: ValDef, _) <- loop(vparam, deep = false)
              vparams1 = vparams.updated(vparams.indexOf(vparam), vparam1)
              vparamss1 = vparamss.updated(vparamss.indexOf(vparams), vparams1)
            } yield AnnotationZipper(ann, vparam1, ddef.copy(vparamss = vparamss1))
            dzippers ++ tzippers ++ vzippers
          }
        case vdef @ ValDef(mods, _, _, _) =>
          mods.annotations.map(ann => {
            val annottee = vdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
        case tdef @ TypeDef(mods, _, tparams, _) =>
          val tzippers = mods.annotations.map(ann => {
            val annottee = tdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
            AnnotationZipper(ann, annottee, annottee)
          })
          if (!deep) tzippers
          else {
            val ttzippers = for {
              tparam <- tparams
              AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
              tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
            } yield AnnotationZipper(ann, tparam1, tdef.copy(tparams = tparams1))
            tzippers ++ ttzippers
          }
        case _ =>
          Nil
      }
      loop(tree, deep = true)
    }

    private object PatchedSyntacticClassDef {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                constrMods: Modifiers, vparamss: List[List[Tree]],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef = {
        // NOTE: works around SI-8771 and hopefully fixes https://github.com/scalamacros/paradise/issues/53 for good
        SyntacticClassDef(mods, name, tparams, constrMods, vparamss.map(_.map(_.duplicate)), earlyDefs, parents, selfType, body)
      }
    }
  }

  case class AnnotationZipper(val annotation: Tree, val annottee: Tree, val owner: Tree)
}
