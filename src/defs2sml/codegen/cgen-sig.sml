signature GEN_C = sig
    type topdecl
    type typeexp = Name.name Type.ty
    type 'a module  = (Name.name, 'a, (Name.name,typeexp) AST.api_info) AST.module
    type 'a module' = (Name.name, 'a, (topdecl*typeexp option) list) AST.module

    type typeinfo

    val generate: typeinfo -> 'a module -> 'a module'
    val print: typeinfo -> TextIO.outstream -> 'a module' -> unit
end