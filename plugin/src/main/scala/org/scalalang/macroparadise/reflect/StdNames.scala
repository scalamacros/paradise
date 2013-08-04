package org.scalalang.macroparadise
package reflect

trait StdNames {
  self: Enrichments =>
  import global._

  implicit class ParadiseNme(nme: global.nme.type) {
    val annottees: TermName          = "annottees"
    val macroTransform: TermName     = "macroTransform"
    val QUASIQUOTE_TUPLE: TermName   = paradisenme.QUASIQUOTE_TUPLE
    val QUASIQUOTE_CASE: TermName    = paradisenme.QUASIQUOTE_CASE
    val QUASIQUOTE_PREFIX: String    = "qq$"
    val QUASIQUOTE_FILE: String      = "<quasiquote>"
    val Apply: TermName              = "Apply"
    val Applied: TermName            = "Applied"
    val Block: TermName              = "Block"
    val EmptyValDefLike: TermName    = "EmptyValDefLike"
    val False : TermName             = "False"
    val FlagsAsBits: TermName        = "FlagsAsBits"
    val New: TermName                = "New"
    val SyntacticClassDef: TermName  = "SyntacticClassDef"
    val TermName: TermName           = "TermName"
    val True : TermName              = "True"
    val TupleN: TermName             = "TupleN"
    val TupleTypeN: TermName         = "TupleTypeN"
    val TypeApplied: TermName        = "TypeApplied"
    val TypeName: TermName           = "TypeName"
    val collection: TermName         = "collection"
    val flatten: TermName            = "flatten"
    val foldLeft: TermName           = "foldLeft"
    val immutable: TermName          = "immutable"
    val mkAnnotationCtor: TermName   = "mkAnnotationCtor"
    val nmeCONSTRUCTOR: TermName     = "CONSTRUCTOR"
    val nmeNme: TermName             = "nme"
    val RefTree: TermName            = "RefTree"
    val toList: TermName             = "toList"
    val CONS                         = encode("::")
    val PLUSPLUS                     = encode("++")
  }

  object paradisenme {
    val q: TermName                  = "q"
    val tq: TermName                 = "tq"
    val cq: TermName                 = "cq"
    val pq: TermName                 = "pq"
    val QUASIQUOTE_TUPLE: TermName   = "$quasiquote$tuple$"
    val QUASIQUOTE_CASE: TermName    = "$quasiquote$case$"
  }

  implicit class ParadiseTpnme(tpnme: global.tpnme.type) {
    val QUASIQUOTE_MODS: TypeName    = paradisetpnme.QUASIQUOTE_MODS
    val QUASIQUOTE_TUPLE: TypeName   = paradisetpnme.QUASIQUOTE_TUPLE
    val FlagSet: TypeName            = "FlagSet"
    val Modifiers: TypeName          = "Modifiers"
    val Option: TypeName             = "Option"
    val CaseDef: TypeName            = "CaseDef"
    val Constant: TypeName           = "Constant"
    val Name: TypeName               = "Name"
    val TermName: TypeName           = "TermName"
    val TypeName: TypeName           = "TypeName"
    val TypeDef: TypeName            = "TypeDef"
    val Tuple: TypeName              = "Tuple"
  }

  object paradisetpnme {
    val QUASIQUOTE_MODS: TypeName    = "$quasiquote$mods$"
    val QUASIQUOTE_TUPLE: TypeName   = "$quasiquote$tuple$"
  }
}
