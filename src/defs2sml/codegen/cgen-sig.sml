signature GEN_C = sig
    type topdecl
    type ty = (Name.name,Name.name) Type.ty
    type 'a module  = (Name.name, 'a, (Name.name,ty) AST.api_info) AST.module
    type 'a module' = (Name.name, 'a, (topdecl*ty option) list) AST.module

    type typeinfo

    val generate: typeinfo -> 'a module -> 'a module'
    val print: typeinfo -> TextIO.outstream -> 'a module' -> unit
end