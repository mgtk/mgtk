(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature ResolveTypes = sig

    type 'a ty = ('a,'a) Type.ty

    val resolve	: (string,AST.api_type) AST.ast_module 
               -> (string,string ty) AST.ast_module

end (* signature ResolveTypes *)
