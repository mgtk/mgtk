(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature ParseUtils =
sig
    type pos = AST.pos
    type declaration = AST.declaration
    type name = NameUtil.name
    type texp = TypeExp.texp
    type inherits = TypeExp.inherits
	
    val mkTypeExp : string -> texp

    val mkWidgetInherits : string option -> inherits
    val mkBoxedInherits : string option option -> inherits option

    val mkModuleDecl : pos * (string * string option) -> declaration list
    val mkWidgetDecl : pos * ((string * inherits) * (texp * string) list option) -> declaration list
    val mkFunDecl    : pos * ((string * texp) * (texp * string) list) -> declaration list
    val mkFlagsDecl  : bool -> pos * (string * string list) -> (declaration list)
    val mkBoxedDecl  : pos * (((string * inherits option) * string list) * string option) -> declaration list
    val mkCBType     : texp * (texp * string) list -> texp
    val mkSignalDecl : pos * ((texp * name) * texp option) -> declaration list
end
