(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature TypeInfo = sig

    type name = Name.name
    type 'a ty = (name, 'a) Type.ty

    type typeinfo
    val build : (name, 'a ty) AST.ast_module -> typeinfo

    exception Unbound of name

    val toSMLType: typeinfo -> (unit->string) -> name ty -> SMLType.ty
    val toSMLTypeSeq: typeinfo -> name ty -> SMLType.ty
    val toPrimType: typeinfo -> 'a ty -> SMLType.ty
    val toSignalType: typeinfo -> 'a ty -> SMLType.ty

    val fromPrimValue: typeinfo -> 'a ty -> TinySML.exp -> TinySML.exp
    val toPrimValue: typeinfo -> name ty -> TinySML.exp -> TinySML.exp

    val isWrapped : typeinfo -> 'a ty -> bool
    val isDefault : typeinfo -> 'a ty -> bool
    val isOutput  : (Type.pass -> bool) -> typeinfo -> 'a ty -> bool
    val isString  : typeinfo -> 'a ty -> bool
    val isBool    : typeinfo -> 'a ty -> bool

    val toCType : typeinfo -> 'a ty -> TinyC.ctype
    val tocvalue: typeinfo -> 'a ty -> string
    val toCValue: typeinfo -> 'a ty -> TinyC.expr -> TinyC.expr
    val fromCValue: typeinfo -> 'a ty -> TinyC.expr -> TinyC.expr

    val defaultValue: typeinfo -> 'a ty -> TinySML.exp

end
