(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature DEPENDENCY_REORDER = sig

    val reorder : (string, (string,'v) Type.ty) AST.ast_module 
		  -> (string, (string,'v) Type.ty) AST.ast_module

end (* signature DEPENDENCY_REORDER *)