package org.scalamacros.paradise
package reflect

import scala.reflect.internal.Flags._
import scala.reflect.internal.MissingRequirementError

trait Definitions {
  self: Enrichments =>

  import global._
  import rootMirror._
  import definitions._

  object paradiseDefinitions {
    private def enterNewMethod(owner: Symbol, name: String, tparams: List[MethodSymbol => Symbol], param: List[Symbol] => Type, ret: List[Symbol] => Type, flags: Long): MethodSymbol = {
      val m = owner.newMethod(newTermName(name), NoPosition, flags)
      val ts = tparams.map(_(m))
      val ps = m.newSyntheticValueParams(List(param(ts)))
      m.setInfoAndEnter(if (ts.isEmpty) MethodType(ps, ret(ts)) else PolyType(ts, MethodType(ps, ret(ts))))
    }

    lazy val InheritedAttr = requiredClass[java.lang.annotation.Inherited]

    lazy val TreeType = {
      if (ApiUniverseClass != NoSymbol) typeRef(ApiUniverseClass.thisPrefix, ApiUniverseClass.info.member(newTypeName("Tree")), Nil)
      else NoType
    }

    lazy val QuasiquoteMacros = {
      // NOTE: we're forced to be binary compatible, therefore
      // we need to approximate the quasiquote API using synthetic symbols:
      //
      //    trait Quasiquotes {
      //      self: scala.reflect.api.Universe =>
      //
      //      implicit class Quasiquote(ctx: StringContext) {
      //        protected trait api {
      //          def apply[T](args: T*): Tree = macro ???
      //          def unapply(subpatterns: Any): Any = macro ???
      //        }
      //        object q extends api
      //        object tq extends api
      //        object cq extends api
      //        object pq extends api
      //        object fq extends api
      //      }
      //   }
      if (ApiUniverseClass != NoSymbol) {
        val impClass = ApiUniverseClass.newClassSymbol(newTypeName("Quasiquote"), NoPosition, IMPLICIT)
        impClass.setInfoAndEnter(ClassInfoType(List(typeOf[AnyRef]), newScope, impClass))
        enterNewMethod(ApiUniverseClass, "Quasiquote", Nil, _ => typeOf[StringContext], _ => impClass.tpe, METHOD | IMPLICIT | SYNTHETIC)

        val flavors = List("q", "tq", "cq", "pq", "fq")
        flavors.flatMap(flavor => {
          val (m, c) = impClass.newModuleAndClassSymbol(newTermName(flavor), NoPosition, 0)
          c.setInfo(ClassInfoType(List(typeOf[AnyRef]), newScope, c))
          val app = enterNewMethod(c, "apply", List(_.newSyntheticTypeParam), ts => scalaRepeatedType(ts.head.tpe), _ => TreeType, MACRO)
          val unapp = enterNewMethod(c, "unapply", Nil, _ => scalaRepeatedType(typeOf[Any]), _ => typeOf[Any], MACRO)
          m.setInfoAndEnter(c.tpe)
          List(app, unapp)
        })
      } else {
        Nil
      }
    }

    lazy val LiftableClass = {
      // trait Liftable[T] {
      //   def apply(value: T): Tree
      // }
      if (ApiUniverseClass != NoSymbol) {
        val liftable = ApiUniverseClass.newClassSymbol(newTypeName("Liftable"), NoPosition, ABSTRACT | INTERFACE | TRAIT)
        val t = liftable.newSyntheticTypeParam
        liftable.setInfoAndEnter(PolyType(List(t), ClassInfoType(List(typeOf[AnyRef]), newScope, liftable)))
        enterNewMethod(liftable, "apply", Nil, _ => t.tpe, _ => TreeType, DEFERRED)
        liftable
      } else {
        NoSymbol
      }
    }

    lazy val UnliftableClass = {
      // trait Unliftable[T] {
      //   def unapply(tree: Tree): Option[T]
      // }
      if (ApiUniverseClass != NoSymbol) {
        val unliftable = ApiUniverseClass.newClassSymbol(newTypeName("Unliftable"), NoPosition, ABSTRACT | INTERFACE | TRAIT)
        val t = unliftable.newSyntheticTypeParam
        unliftable.setInfoAndEnter(PolyType(List(t), ClassInfoType(List(typeOf[AnyRef]), newScope, unliftable)))
        enterNewMethod(unliftable, "unapply", Nil, _ => TreeType, _ => appliedType(OptionClass, t.tpe), DEFERRED)
        unliftable
      } else {
        NoSymbol
      }
    }

    lazy val QuasiquoteCompatModule = getModuleIfDefined("scala.quasiquotes.QuasiquoteCompat")

    private def termPath(fullname: String): Tree = {
      val parts = fullname split "\\."
      val prefixParts = parts.init
      val lastName = TermName(parts.last)
      if (prefixParts.isEmpty) Ident(lastName)
      else {
        val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
        Select(prefixTree, lastName)
      }
    }

    lazy val QuasiquoteCompatModuleRef = termPath(QuasiquoteCompatModule.fullName)

    lazy val QuasiquoteCompatBuild = getClassIfDefined("scala.quasiquotes.QuasiquoteCompat").info.decl(newTermName("build"))

    def QuasiquoteCompatBuildRef(universe: Tree) = {
      // NOTE: density of workarounds per line of code is rapidly ramping up!
      // here we force the compiler into supporting path-dependent extractors
      // by manually applying the first argument list, i.e. the one that contains the universe,
      // and then setting the type of the resulting Apply node so that the typechecker doesn't touch it later
      // if we didn't set the type, then typedApply for the first arglist would get confused and fai
      val compatModule = QuasiquoteCompatModule.moduleClass
      val compatApply = compatModule.info.decl(nme.apply)
      val applyWithoutTargs = gen.mkAttributedRef(compatModule.tpe, compatApply)
      val targs = List(SingletonTypeTree(universe) setType universe.tpe)
      val applyWithTargs = TypeApply(applyWithoutTargs, targs) setType appliedType(applyWithoutTargs.tpe, List(universe.tpe))
      val compatInstance = Apply(applyWithTargs, List(universe)) setType applyWithTargs.tpe.resultType(List(universe.tpe))
      Select(compatInstance, nme.build)
    }

    class UniverseDependentTypes(universe: Tree) {
      lazy val nameType         = universeMemberType(tpnme.Name)
      lazy val modsType         = universeMemberType(tpnme.Modifiers)
      lazy val flagsType        = universeMemberType(tpnme.FlagSet)
      lazy val symbolType       = universeMemberType(tpnme.Symbol)
      lazy val treeType         = universeMemberType(tpnme.Tree)
      lazy val caseDefType      = universeMemberType(tpnme.CaseDef)
      lazy val liftableType     = universeMemberType(tpnme.Liftable)
      lazy val unliftableType   = universeMemberType(tpnme.Unliftable)
      lazy val iterableTreeType = appliedType(IterableClass, treeType)
      lazy val listTreeType     = appliedType(ListClass, treeType)
      lazy val listListTreeType = appliedType(ListClass, listTreeType)
      def universeMemberType(name: TypeName) = universe.tpe.memberType(getTypeMember(universe.symbol, name))
    }

    def isListType(tp: Type)     = tp <:< classExistentialType(ListClass)
    def isIterableType(tp: Type) = tp <:< classExistentialType(IterableClass)

    def init() = {
      QuasiquoteMacros
      LiftableClass
      UnliftableClass
    }
  }
}
