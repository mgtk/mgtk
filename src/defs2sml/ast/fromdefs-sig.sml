(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature FromDefs = sig

    val fromDefs: string (* toplevel module name *) 
		  -> Defs.definition list -> Defs.metadata list
                  -> (string,AST.api_type) AST.ast_module

end (* signature FromDefs *)