(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature FromDefs = sig

    type module_info = (AST.api_type * AST.api_type option) option
    type member_info = AST.api_type AST.api_info
    val fromDefs: string -> Defs.definition list 
                  -> (string,module_info,member_info) AST.module

end (* signature FromDefs *)