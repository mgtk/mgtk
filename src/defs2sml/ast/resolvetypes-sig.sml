(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature ResolveTypes = sig

    type 'a ty = 'a Type.ty
    type module_info = (string * string option * string list) option
    type 'a member_info = (string,'a) AST.api_info

    val resolve: (string,module_info,AST.api_type member_info) AST.module 
                 -> (string,module_info,string ty member_info) AST.module

end (* signature ResolveTypes *)
