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
    val mkCBType     : texp * (texp*string) list -> texp

    val mkWidgetInherits : string option -> inherits
    val mkBoxedInherits : string option option -> inherits option

    val mkModuleDecl : pos * (string*string option) 
                     -> declaration list
    val mkWidgetDecl : pos * ((string*inherits) * (texp*string) list option) 
                     -> declaration list
    val mkFunDecl    : pos * ((string*texp) * (texp*string) list) 
                     -> declaration list
    val mkFlagsDecl  : bool -> pos * (string*string list) 
                     -> (declaration list)
    val mkBoxedDecl  : pos*(((string*inherits option)*string list)*string option) 
                     -> declaration list
    val mkSignalDecl : pos * ((texp*name) * texp option) 
                     -> declaration list

end

(* 
    This module provides utility functions to the parser. The
    functions construct abstract syntax from the parsed strings.

    [mkTypeExp tname] resolves the type name tname into a type
    expression.

    [mkCBType rettexp pars] constructs a type expression representing
    a callback function with arguments pars and return type rettexp.

    [mkWidgetInherits parent] constructs an inheritance specification
    for a widget. If parent is NONE, this would be the top of the
    inheritance hierarchy, otherwise it specifies the parent of the
    widget.

    [mkBoxedInherits parent] as above. However, since not all boxed
    types have an associated inheritance hierarchy includes an extra
    level of optionality: if parent is NONE there is no inheritance,
    if parent is SOME NONE this is the top of the hierarchy, if parent
    is SOME (SOME p) this specifies the parent of the boxed type as p.

    [mkXXXDecl] constructs a declaration of type XXX from the relevant
    subparts.

*)
