signature ResolveNames = sig

    type 'a ty = ('a,'a) Type.ty
    type name = Name.name

    val resolve 
        : (string,string ty) AST.ast_module -> (name,name ty) AST.ast_module

end (* signature ResolveNames *)