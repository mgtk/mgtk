(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature TypeInfo = sig

    type name = Name.name
    type ty = name Type.ty

    type info = {stype: (unit -> string) -> SMLType.ty, ptype: SMLType.ty,
		 toc: TinyC.expr -> TinyC.expr,
		 fromc: TinyC.expr -> TinyC.expr,
		 super: name option}
    type typeinfo
    val build: (name,(ty * name option)option,ty AST.api_info) AST.module 
	       -> typeinfo

    exception Unbound of name

    val toSMLType: typeinfo -> (unit->string) -> ty -> SMLType.ty
    val toSMLTypeSeq: typeinfo -> ty -> SMLType.ty
    val toPrimType: typeinfo -> ty -> SMLType.ty

    val toCValue: typeinfo -> ty -> TinyC.expr -> TinyC.expr
    val fromCValue: typeinfo -> ty -> TinyC.expr -> TinyC.expr

end
