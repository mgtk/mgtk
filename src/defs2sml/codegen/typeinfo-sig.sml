(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature TypeInfo = sig

    type name = Name.name
    type 'a ty = (name, 'a) Type.ty

    type info 
(*
         = {stype: (unit -> string) -> SMLType.ty, ptype: SMLType.ty,
	    toc: TinyC.expr -> TinyC.expr,
	    fromc: TinyC.expr -> TinyC.expr,
	    super: name option,
	    fromprim: TinySML.exp->TinySML.exp, toprim: TinySML.exp->TinySML.exp}
*)
    type typeinfo
    val build: (name,(name * name option * name list)option,('a,'b ty) AST.api_info) AST.module 
	       -> typeinfo

    exception Unbound of name

    val toSMLType: typeinfo -> (unit->string) -> 'a ty -> SMLType.ty
    val toSMLTypeSeq: typeinfo -> 'a ty -> SMLType.ty
    val toPrimType: typeinfo -> 'a ty -> SMLType.ty

    val fromPrimValue: typeinfo -> 'a ty -> TinySML.exp -> TinySML.exp

    val isWrapped: typeinfo -> 'a ty -> bool
    val isDefault : typeinfo -> 'a ty -> bool
    val isString : typeinfo -> 'a ty -> bool

    val toCValue: typeinfo -> 'a ty -> TinyC.expr -> TinyC.expr
    val fromCValue: typeinfo -> 'a ty -> TinyC.expr -> TinyC.expr

end
