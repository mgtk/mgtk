signature ResolveNames = sig

    type 'a ty = 'a Type.ty
    type name = Name.name

    type 'a module_info = ('a * 'a option) option
    type 'a member_info = ('a,'a ty) AST.api_info

(*
    val toName: (string -> string list) -> string list -> string
                -> (string list * string list)
*)
    val resolve: (string,string ty module_info,string member_info) AST.module 
                 -> (name,name module_info,name member_info) AST.module

end (* signature ResolveNames *)