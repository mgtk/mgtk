(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature GEN_SML = sig
    type name = Name.name
    type typeexp = name Type.ty

    type sml_info
    type 'a incl
    type module_info

    type typeinfo

    val generate: typeinfo -> (name,(name*name option)option,(name,typeexp) AST.api_info) AST.module -> 
		      (name,module_info,sml_info incl) AST.module
    val print: string option -> TextIO.outstream -> (name,module_info,sml_info incl) AST.module 
                   -> unit
end
