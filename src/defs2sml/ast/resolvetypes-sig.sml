(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature ResolveTypes = sig

    type module_info = (AST.api_type * AST.api_type option) option
    type member_info = AST.api_type AST.api_info

    type ty = Name.name Type.ty
    type module_info' = (ty * ty option) option
    type member_info' = ty AST.api_info

    val resolve: (Name.name,module_info,member_info) AST.module 
                 -> (Name.name,module_info',member_info') AST.module

end (* signature ResolveTypes *)
