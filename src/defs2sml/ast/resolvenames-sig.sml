signature ResolveNames = sig

    val splitWords: string -> string list
    val splitUnderscores: string -> string list
    val toName: (string -> string list * string) -> string list -> string
                -> (string list * string)

    val resolve: (string,'i1,'i2) AST.module -> (Name.name,'i1,'i2) AST.module
(*
    val modularize: (Name.name,'i1,'i2) AST.module -> (Name.name,'i1,'i2) AST.module
*)

end (* signature ResolveNames *)