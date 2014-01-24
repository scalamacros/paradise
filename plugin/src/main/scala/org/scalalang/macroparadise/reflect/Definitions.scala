package org.scalalang.macroparadise
package reflect

import scala.reflect.internal.Flags._
import scala.reflect.internal.MissingRequirementError

trait Definitions {
  self: Enrichments =>

  import global._
  import rootMirror._
  import definitions._

  object paradiseDefinitions {
    lazy val InheritedAttr = requiredClass[java.lang.annotation.Inherited]

    lazy val QuasiquoteMacros = {
      // NOTE: we're forced to be binary compatible, therefore
      // we need to approximate the quasiquote API using synthetic symbols:
      //
      //    trait Quasiquotes {
      //      self: scala.reflect.api.Universe =>
      //
      //      implicit class Quasiquote(ctx: StringContext) {
      //        protected trait api {
      //          def apply(args: Any*): Any = macro ???
      //          def unapply(subpatterns: Any*): Option[Any] = macro ???
      //        }
      //        object q extends api
      //        object tq extends api
      //        object cq extends api
      //        object pq extends api
      //      }
      //   }
      if (ApiUniverseClass != NoSymbol) {
        def enterNewMethod(owner: Symbol, name: String, param: Type, ret: Type, flags: Long): MethodSymbol = {
          val m = owner.newMethod(newTermName(name), NoPosition, flags)
          val ps = m.newSyntheticValueParams(List(param))
          m.setInfoAndEnter(MethodType(ps, ret))
        }

        val impClass = ApiUniverseClass.newClassSymbol(newTypeName("Quasiquote"), NoPosition, IMPLICIT)
        impClass.setInfoAndEnter(ClassInfoType(List(typeOf[AnyRef]), newScope, impClass))
        enterNewMethod(ApiUniverseClass, "Quasiquote", typeOf[StringContext], impClass.tpe, METHOD | IMPLICIT | SYNTHETIC)

        val flavors = List("q", "tq", "cq", "pq")
        flavors.flatMap(flavor => {
          val (m, c) = impClass.newModuleAndClassSymbol(newTermName(flavor), NoPosition, 0)
          c.setInfo(ClassInfoType(List(typeOf[AnyRef]), newScope, c))
          val app = enterNewMethod(c, "apply", scalaRepeatedType(typeOf[Any]), typeOf[Any], MACRO)
          val unapp = enterNewMethod(c, "unapply", scalaRepeatedType(typeOf[Any]), typeOf[Option[Any]], MACRO)
          m.setInfoAndEnter(c.tpe)
          List(app, unapp)
        })
      } else {
        Nil
      }
    }

    lazy val QuasiquoteCompatModule = getModuleIfDefined("org.scalalang.quasiquotes.QuasiquoteCompat")
    lazy val LiftableClass = getClassIfDefined("org.scalalang.quasiquotes.Liftable")

    class UniverseDependentTypes(universe: Tree) {
      lazy val universeType = universe.tpe
      lazy val universeSym = universe.symbol
      lazy val nameType = universeMemberType(tpnme.Name)
      lazy val termNameType = universeMemberType(tpnme.TypeName)
      lazy val typeNameType = universeMemberType(tpnme.TermName)
      lazy val modsType = universeMemberType(tpnme.Modifiers)
      lazy val flagsType = universeMemberType(tpnme.FlagSet)
      lazy val symbolType = universeMemberType(tpnme.Symbol)
      lazy val treeType = universeMemberType(tpnme.Tree)
      lazy val typeDefType = universeMemberType(tpnme.TypeDef)
      lazy val caseDefType = universeMemberType(tpnme.CaseDef)
      lazy val iterableTreeType = appliedType(IterableClass, treeType)
      lazy val iterableCaseDefType = appliedType(IterableClass, caseDefType)
      lazy val iterableIterableTreeType = appliedType(IterableClass, iterableTreeType)
      lazy val listTreeType = appliedType(ListClass, treeType)
      lazy val listListTreeType = appliedType(ListClass, listTreeType)
      lazy val optionTreeType = appliedType(OptionClass, treeType)
      lazy val optionNameType = appliedType(OptionClass, nameType)
      lazy val typeType = universeMemberType(tpnme.Type)
      lazy val constantType = universeMemberType(tpnme.Constant)
      lazy val weakTypeTagType = universeMemberType(tpnme.WeakTypeTag)
      lazy val exprType = universeMemberType(tpnme.Expr)
      def universeMemberType(name: TypeName) = {
        val tpe = universe.tpe.memberType(getTypeMember(universe.symbol, name))
        if (tpe.typeSymbol.typeParams.isEmpty) tpe
        else newExistentialType(tpe.typeSymbol.typeParams, tpe)
      }
    }

    def isLiftableType(tp: Type) = tp <:< classExistentialType(LiftableClass)
    def isIterableType(tp: Type) = tp <:< classExistentialType(IterableClass)

    def init() = {
      QuasiquoteMacros
    }
  }
}
