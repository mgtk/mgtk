(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature GEN_SML = sig
    type name = Name.name
    type ty = (name,name) Type.ty
    type typeinfo

    val translate: typeinfo -> (name,(name*name option*name list)option,(name,ty)AST.api_info) AST.module -> TinySML.topdec
    val print: string option -> bool -> TextIO.outstream -> TinySML.topdec -> unit
end
