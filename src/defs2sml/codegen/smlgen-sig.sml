(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature GEN_SML = sig
    type name = Name.name
    type ty = (name,name) Type.ty
    type typeinfo

    val translate: bool -> typeinfo -> (name,ty) AST.ast_module -> TinySML.topdec
    val print: string option -> bool -> TextIO.outstream -> TinySML.topdec -> unit
end
