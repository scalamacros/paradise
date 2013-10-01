package org.scalalang.macroparadise
package reflect

trait StdNames {
  self: Enrichments =>
  import global._

  implicit class ParadiseNme(nme: global.nme.type) {
    val annottees: TermName            = "annottees"
    val Apply: TermName                = "Apply"
    val CONS: TermName                 = encode("::")
    val collection: TermName           = "collection"
    val EmptyValDefLike: TermName      = "EmptyValDefLike"
    val False : TermName               = "False"
    val flatten: TermName              = "flatten"
    val foldLeft: TermName             = "foldLeft"
    val FlagsRepr: TermName            = "FlagsRepr"
    val immutable: TermName            = "immutable"
    val macroTransform: TermName       = "macroTransform"
    val mkAnnotation: TermName         = "mkAnnotation"
    val mkRefineStat: TermName         = "mkRefineStat"
    val mkEarlyDef: TermName           = "mkEarlyDef"
    val New: TermName                  = "New"
    val NoMods: TermName               = "NoMods"
    val PLUSPLUS: TermName             = encode("++")
    val QUASIQUOTE_TUPLE: TermName     = paradisenme.QUASIQUOTE_TUPLE
    val QUASIQUOTE_CASE: TermName      = paradisenme.QUASIQUOTE_CASE
    val QUASIQUOTE_PREFIX: String      = "qq$"
    val QUASIQUOTE_FILE: String        = "<quasiquote>"
    val RefTree: TermName              = "RefTree"
    val ScalaDot: TermName             = "ScalaDot"
    val SyntacticApplied: TermName     = "SyntacticApplied"
    val SyntacticAssign: TermName      = "SyntacticAssign"
    val SyntacticBlock: TermName       = "SyntacticBlock"
    val SyntacticClassDef: TermName    = "SyntacticClassDef"
    val SyntacticDefDef: TermName      = "SyntacticDefDef"
    val SyntacticFunction: TermName    = "SyntacticFunction"
    val SyntacticFunctionType: TermName= "SyntacticFunctionType"
    val SyntacticModuleDef: TermName   = "SyntacticModuleDef"
    val SyntacticNew: TermName         = "SyntacticNew"
    val SyntacticTraitDef: TermName    = "SyntacticTraitDef"
    val SyntacticTuple: TermName       = "SyntacticTuple"
    val SyntacticTupleType: TermName   = "SyntacticTupleType"
    val SyntacticTypeApplied: TermName = "SyntacticTypeApplied"
    val SyntacticValDef: TermName      = "SyntacticValDef"
    val SyntacticVarDef: TermName      = "SyntacticVarDef"
    val TermName: TermName             = "TermName"
    val True : TermName                = "True"
    val TypeName: TermName             = "TypeName"
    val toList: TermName               = "toList"
  }

  object paradisenme {
    val q: TermName                = "q"
    val tq: TermName               = "tq"
    val cq: TermName               = "cq"
    val pq: TermName               = "pq"
    val QUASIQUOTE_TUPLE: TermName = "$quasiquote$tuple$"
    val QUASIQUOTE_CASE: TermName  = "$quasiquote$case$"
  }

  implicit class ParadiseTpnme(tpnme: global.tpnme.type) {
    val QUASIQUOTE_MODS: TypeName        = paradisetpnme.QUASIQUOTE_MODS
    val QUASIQUOTE_TUPLE: TypeName       = paradisetpnme.QUASIQUOTE_TUPLE
    val QUASIQUOTE_FUNCTION: TypeName    = paradisetpnme.QUASIQUOTE_FUNCTION
    val QUASIQUOTE_REFINE_STAT: TypeName = paradisetpnme.QUASIQUOTE_REFINE_STAT
    val QUASIQUOTE_EARLY_DEF: TypeName   = paradisetpnme.QUASIQUOTE_EARLY_DEF
    val FlagSet: TypeName                = "FlagSet"
    val Modifiers: TypeName              = "Modifiers"
    val Option: TypeName                 = "Option"
    val CaseDef: TypeName                = "CaseDef"
    val Constant: TypeName               = "Constant"
    val Name: TypeName                   = "Name"
    val TermName: TypeName               = "TermName"
    val TypeName: TypeName               = "TypeName"
    val TypeDef: TypeName                = "TypeDef"
    val Tuple: TypeName                  = "Tuple"
  }

  object paradisetpnme {
    val QUASIQUOTE_MODS: TypeName        = "$quasiquote$mods$"
    val QUASIQUOTE_TUPLE: TypeName       = "$quasiquote$tuple$"
    val QUASIQUOTE_FUNCTION: TypeName    = "$quasiquote$function$"
    val QUASIQUOTE_REFINE_STAT: TypeName = "$quasiquote$refine$stat$"
    val QUASIQUOTE_EARLY_DEF: TypeName   = "$quasiquote$early$def$"
  }
}
