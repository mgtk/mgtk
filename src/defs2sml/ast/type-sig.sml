(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature TYPE = sig

    datatype pass = OUT | INOUT (* no need to have IN as that is the default *)

    datatype ('n, 'v) ty =
	Void
      | Base of 'n
      | Tname of 'n
      | Ptr of ('n,'v) ty
      | Const of ('n,'v) ty
      | Array of ('n,'v) ty
      | Func of (string * ('n,'v) ty) list * ('n,'v) ty
      | WithDefault of ('n,'v) ty * 'v
      | Output of pass * ('n,'v) ty

    val pp : 'n Pretty.pp -> 'v Pretty.pp -> ('n,'v) ty Pretty.pp
    val toString : 'n Pretty.pp -> 'v Pretty.pp -> ('n,'v) ty -> string

    val map: ('n1 -> 'n2) -> ('n1,'v) ty -> ('n2,'v) ty
    val mapi: ((('n1,'v) ty * 'n1) -> 'n2) -> ('n1,'v) ty -> ('n2,'v) ty
    val mapiv: ((('n1,'v1) ty * 'n1) -> 'n2) -> (('n1,'v1) ty * 'v1 -> 'v2)
             -> ('n1,'v1) ty -> ('n2,'v2) ty

    val getParams: ('n,'v) ty -> (string * ('n,'v) ty) list
    val getRetType: ('n,'v) ty -> ('n,'v) ty

end (* signature Type *)
